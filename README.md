# Flatiron

Flatiron is a columnar analytics library for Clojure. It lets you run fast analytical queries on in-memory tables using a SQL-like DSL, and it handles graph algorithms on the same data. It's pure Clojure with no dependencies beyond core.async.

Think of it as what you'd reach for instead of dragging in a full embedded database: load some data, run group-by aggregations, sort and filter, maybe run PageRank on a graph, all in-process with zero configuration.

## Why columnar

Most Clojure programs represent tabular data as sequences of maps. That's fine for a few thousand rows, but it falls apart on larger datasets: every row is a heap-allocated map, every value is boxed, and every access goes through layers of indirection.

Flatiron stores data as typed primitive arrays — one array per column. An integer column is a `long[]`, a float column is a `double[]`, and so on. Operations loop over these arrays directly using unchecked arithmetic, which the JVM can optimize into tight native code. Nulls are handled with sentinel values rather than boxed types, so there's no pointer chasing.

The morsel engine processes data in 1024-row batches. This amortizes the cost of type dispatch: decide the operation once per batch, then run a tight loop over primitives. The result is performance closer to native C than to idiomatic Clojure.

## Installation

Flatiron has no published artifact yet. For now, add the git dependency to your `deps.edn`:

```clojure
{io.github.yogthos/flatiron {:git/sha "..."}}
```

It depends on Clojure 1.12.0 and core.async 1.6.681.

## Concepts

### Columns and tables

There are five column types. Each stores data as a Java primitive array with optional null sentinels:

- **I64** — signed 64-bit integers (`long[]`)
- **F64** — 64-bit floats (`double[]`)
- **Bool** — booleans (`byte[]`)
- **Sym** — Clojure keywords (`Object[]`)
- **Str** — strings (`Object[]`)

A **Table** is a schema (vector of keyword column names) plus a vector of columns. That's it — no metadata, no indexes, just typed arrays with names.

```clojure
(require '[flatiron.column :as col])
(require '[flatiron.table :as tbl])

(let [symbols (col/sym-column ["AAPL" "GOOG" "MSFT" "AAPL" "GOOG"])
      qty     (col/i64-column [100    200    150    300    250])
      table   (tbl/table [:Symbol :Qty] [symbols qty])]

  (tbl/nrows table)  ;; => 5
  (tbl/ncols table)  ;; => 2
  (tbl/col table :Qty))  ;; => #<I64Column ...>
```

### Filtering

The `where` macro builds a boolean mask by comparing a column against a constant, combines masks for compound predicates with `and`, `or`, and `not`, then materializes the rows that pass into a new table. A three-level selection bitmap lives in `flatiron.selection` as a lower-level primitive for callers that want to track which rows are alive without materializing, but the built-in `where` materializes eagerly.

### Morsel engine

Named after the Rayforce concept of a "morsel" (a bite-sized piece of data). All compute operations follow the same pattern: create a morsel source from a column, then pull 1024-row batches through it. Within each batch, the loop body runs over raw primitive arrays with no protocol dispatch.

This gives you the abstraction of typed generic operations with the performance of hand-written primitive loops.

## The DSL

The DSL compiles to morsel-based operations at macro expansion time. There's no runtime query parsing — the macros emit direct function calls.

```clojure
(require '[flatiron.dsl :refer [sum count avg min max]])

;; Group-by with aggregation
(select trades :Symbol (sum :Qty))

;; Multiple aggregate functions
(select trades :Symbol (sum :Qty) (avg :Price) (count :Qty))

;; Multiple group-by keys
(select trades :Region :Side (sum :Qty))

;; Cross-tabulation
(pivot trades :Symbol :Side :Qty sum)
```

### Filters

```clojure
(-> trades
    (where (> :Qty 100))
    (select :Symbol (sum :Qty)))
```

This filters to rows where Qty > 100, then groups by Symbol and sums Qty. The `where` macro compiles the predicate to a mask-building call that dispatches on the column's runtime type, so an integer literal compared against a float column is coerced automatically rather than picking the comparison from the literal's type.

Supported predicates: `>`, `<`, `>=`, `<=`, `=`, `not=`, combined with `and`, `or`, and `not`.

## Aggregation functions

All aggregations are single-pass morsel reductions. They dispatch on column type once, then loop over primitive arrays with unchecked arithmetic.

| Function | Description |
|----------|-------------|
| `sum` | Sum of values, skips nulls |
| `count` | Count of non-null values |
| `avg` | Arithmetic mean, skips nulls |
| `min` | Minimum value |
| `max` | Maximum value |

## Sorting and window functions

Sorting uses `java.util.TimSort` on an index array — the column data is never permuted. Sorting is stable and supports ascending and descending order.

```clojure
(require '[flatiron.sort :as sort])
(require '[flatiron.window :as win])

(let [sorted (sort/sort-by table :Qty :asc)]
  (win/row-number sorted))  ;; => I64Column [1 2 3 4 5]
```

Window functions operate on sorted columns:

- `row-number` — sequential 1-based numbering
- `rank` — rank with gaps after ties (1, 1, 3, 3, 5)
- `dense-rank` — rank without gaps (1, 1, 2, 2, 3)
- `lag` / `lead` — access previous or next row's value with offset and default

## Parallel execution

CPU-bound aggregations can run across multiple threads using core.async. Each thread processes an independent slice of rows using the same morsel engine, and results are merged at the end.

```clojure
(require '[flatiron.parallel :as par])

(par/parallel-i64-sum qty-col 4)     ;; sum across 4 threads
(par/parallel-group-by table :keys [:Region] :aggs ...)  ;; parallel group-by
```

The parallelism is transparent — you get the same result as single-threaded, just faster on large datasets.

## Graph algorithms

Flatiron includes a CSR (Compressed Sparse Row) graph engine. You build a graph from two columns (source and target node IDs), and it constructs forward and reverse adjacency structures in a single pass.

```clojure
(require '[flatiron.graph :as g])

(let [src    (col/i64-column [0 0 1 2 3])
      dst    (col/i64-column [1 2 3 3 0])
      graph  (g/graph src dst)
      result (g/page-rank graph 20 0.85)]
  ;; result is a table with :node and :rank columns
  )
```

Available algorithms:

- **BFS** — breadth-first search from a start node
- **DFS** — depth-first search, iterative (not recursive)
- **Dijkstra** — single-source shortest paths with weights
- **PageRank** — iterative PageRank with configurable damping factor and iterations
- **Connected components** — finds weakly connected components via BFS

## I/O

### CSV

The CSV reader does type inference by sampling the first 100 rows, then reads into typed columns. It handles a wide range of types automatically.

```clojure
(require '[flatiron.io :as io])

(let [table (io/read-csv "data/trades.csv")]
  (select table :Symbol (sum :Qty)))
```

The writer outputs a table back to CSV.

### Binary columnar store

For fast persistence, Flatiron has a column-per-file binary format. Each column becomes a file (`col_Symbol`, `col_Qty`, ...) with a small `_meta.edn` describing the schema. Reading is zero-copy where possible.

```clojure
(require '[flatiron.store :as store])

(store/save-table table "data/trades_store")
(let [loaded (store/load-table "data/trades_store")]
  (select loaded :Symbol (sum :Qty)))
```

## Why Flatiron over alternatives

There are several good Clojure data libraries. Flatiron fills a specific niche:

- **vs `clojure.core` seq operations** — Flatiron is two to three orders of magnitude faster on numeric aggregates over many rows, but it only works with its own column types. Use core functions for quick scripting, Flatiron for the heavy lifting.

- **vs `tech.ml.dataset`** — TMD is more feature-rich (date handling, statistical functions, interop with many formats). Flatiron is smaller, has no native dependencies, and focuses on raw speed for a narrower set of operations.

- **vs embedded databases (H2, SQLite)** — Databases give you SQL, transactions, and persistence. Flatiron gives you in-process data you can manipulate directly from Clojure without going through JDBC. If you already have data in Clojure data structures and just need fast analytics, Flatiron is less ceremony.

## Acknowledgments

Flatiron is a Clojure reimplementation of ideas from [Rayforce](https://github.com/RayforceDB/rayforce), a SIMD-accelerated columnar analytics and graph engine written in C17. The morsel-driven execution model, the 1024-element batch size, the CSR graph layout with its BFS, DFS, Dijkstra, and PageRank algorithms, and the SQL-like surface all follow Rayforce's design. The wyhash hashing in `flatiron.hash` is ported from Rayforce's `src/ops/hash.h`, and the DSL borrows from its Rayfall query language. Rayforce is MIT licensed.

## License

MIT
