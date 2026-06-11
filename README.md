# Flatiron

<p align="center">
  <img src="img/logo.svg" alt="Flatiron logo" width="400" />
</p>

Flatiron is a columnar analytics library for Clojure. It lets you run fast analytical queries on in-memory tables using a SQL-like DSL, and it handles graph algorithms on the same data. It's pure Clojure with no dependencies beyond core.async.

Think of it as what you'd reach for instead of dragging in a full embedded database: load some data, run group-by aggregations, sort and filter, maybe run PageRank on a graph, all in-process with zero configuration.

## Why columnar

Most Clojure programs represent tabular data as sequences of maps. That's fine for a few thousand rows, but it falls apart on larger datasets: every row is a heap-allocated map, every value is boxed, and every access goes through layers of indirection.

Flatiron stores data as typed primitive arrays — one array per column. An integer column is a `long[]`, a float column is a `double[]`, and so on. Operations loop over these arrays directly using unchecked arithmetic, which the JVM can optimize into tight native code. Nulls are handled with sentinel values rather than boxed types, so there's no pointer chasing.

The morsel engine processes data in 1024-row batches. This amortizes the cost of type dispatch: decide the operation once per batch, then run a tight loop over primitives. The result is performance closer to native C than to idiomatic Clojure.

## Installation

Add the git dependency to your `deps.edn`:

```clojure
{io.github.yogthos/flatiron {:git/tag "v0.2.0"
                             :git/sha "98d700ee79b5425cd837db5b7866a69cf4a0f432"}}
```

It depends on Clojure 1.12.0 and core.async 1.6.681, and requires JDK 18+
(the hash kernels use `Math/unsignedMultiplyHigh`); CI runs on 21 and 25.

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

(let [dragons (col/sym-column [:smaug :fafnir :tiamat :smaug :fafnir])
      gold    (col/i64-column [9000   750     1200    3100   2400])
      table   (tbl/table [:Dragon :Gold] [dragons gold])]

  (tbl/nrows table)  ;; => 5
  (tbl/ncols table)  ;; => 2
  (tbl/col table :Gold))  ;; => #<I64Column ...> — Smaug is hoarding again
```

### Custom types

Domain types like dates and timestamps don't need their own column type or an `Object[]` that boxes every value. A `LocalDate` is just its epoch day, an `Instant` is just its epoch milli, and those encodings preserve order, so comparison, sorting, group-by, and min/max are all already correct on the underlying primitive. Flatiron stores such a type as a normal `long[]` column tagged with a *logical type*: the column reports its physical type (`:i64`) to every operation, so the hot loops are unchanged and the values are never boxed, and only the boundaries (building a column, reading a value back, persisting) run the codec that converts to and from the domain object.

```clojure
(require '[flatiron.column :as col])

(let [hired (col/date-column [(java.time.LocalDate/of 2019 4 1)
                              (java.time.LocalDate/of 2021 9 15)])]
  (col/-type-tag hired)        ;; => :i64   (physical — what operations dispatch on)
  (col/-logical-tag hired)     ;; => :date  (logical — how values are exposed)
  (col/-get-obj hired 0))      ;; => #object[java.time.LocalDate "2019-04-01"]
```

Built-in logical types, all backed by `long[]`: `:date` (`LocalDate`), `:instant` (`Instant`), `:datetime` (`LocalDateTime`), `:date-millis` (`java.util.Date`), and `:duration` (`Duration`). Build a column for any of them with `col/typed-column` or the `date-column`/`instant-column`/`datetime-column`/`duration-column` helpers.

Predicates take the domain value directly. The literal is encoded once per predicate, outside the row loop, so the comparison stays a primitive operation:

```clojure
(-> employees
    (where (>= :Hired (java.time.LocalDate/of 2020 1 1)))
    (select :Dept (count :Hired)))
```

Operations that preserve a value keep the logical type: filtering, sorting, group-by keys, and `min`/`max` of a date column all return dates. Aggregations that produce a new number drop it, so `sum` and `avg` over a date column come back as plain `:i64`/`:f64`. The binary store records the logical type in its metadata, so saved tables round-trip with their types intact.

Register your own type with `flatiron.types/register-type!`, giving it a physical backing (`:i64` or `:f64`) and an encode/decode pair:

```clojure
(require '[flatiron.types :as types])

(types/register-type! :cents
  {:physical :i64
   :class    java.math.BigDecimal
   :encode   (fn ^long [^java.math.BigDecimal d] (.longValueExact (.movePointRight d 2)))
   :decode   (fn [^long v] (.movePointLeft (java.math.BigDecimal/valueOf v) 2))})

(col/typed-column :cents [(bigdec "1.50") (bigdec "2.25")])
```

### Filtering

The `where` macro builds a boolean mask by comparing a column against a constant, combines masks for compound predicates with `and`, `or`, and `not`, then materializes the rows that pass into a new table. A three-level selection bitmap lives in `flatiron.selection` as a lower-level primitive for callers that want to track which rows are alive without materializing, but the built-in `where` materializes eagerly.

For filter-then-aggregate pipelines, `flatiron.group/group-by` and `parallel-group-by` accept the mask directly via a `:where` option and gather only the key and aggregate columns through it, skipping the intermediate table entirely.

### Morsel engine

Named after the Rayforce concept of a "morsel" (a bite-sized piece of data). Element-wise operations (arithmetic, comparisons) create a morsel source from a column and pull 1024-row batches through it; within each batch, the loop body runs over raw primitive arrays with no protocol dispatch.

Aggregations and group-by go one step further: they read the column's backing array directly in type-specialized loops, falling back to the morsel layer only where the indirection is needed. Either way you get the abstraction of typed generic operations with the performance of hand-written primitive loops.

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

All aggregations are single-pass reductions that dispatch on column type once, then loop directly over the backing primitive arrays with unchecked arithmetic.

| Function | Description |
|----------|-------------|
| `sum` | Sum of values, skips nulls |
| `count` | Count of non-null values |
| `avg` | Arithmetic mean, skips nulls; null for groups with no non-null values |
| `min` | Minimum value, skips nulls; null for groups with no non-null values |
| `max` | Maximum value, skips nulls; null for groups with no non-null values |

## Sorting and window functions

Sorting uses `java.util.TimSort` on an index array — the column data is never permuted. Sorting is stable and supports ascending and descending order.

```clojure
(require '[flatiron.sort :as sort])
(require '[flatiron.window :as win])

(let [sorted (sort/sort-table table [[:Qty :asc]])]
  (win/row-number sorted))  ;; => I64Column [1 2 3 4 5]
```

Window functions operate on sorted columns:

- `row-number` — sequential 1-based numbering
- `rank` — rank with gaps after ties (1, 1, 3, 3, 5)
- `dense-rank` — rank without gaps (1, 1, 2, 2, 3)
- `lag` / `lead` — access previous or next row's value with offset and default

## Parallel execution

The main parallel entry point is `flatiron.group/parallel-group-by`: it radix-partitions rows by the high bits of the key hash and runs the hash, histogram, scatter, and per-partition grouping phases on a shared ForkJoin pool. Partitions are disjoint, so results concatenate without a merge step, and output is identical to the single-threaded `group-by`.

```clojure
(require '[flatiron.group :as g])

(g/parallel-group-by table
  :keys [:Region]
  :aggs [{:agg :sum :col :Qty :out :total}]
  :n-threads 8)
```

`flatiron.parallel` additionally provides per-column parallel primitives (`parallel-i64-sum`, `parallel-i64-min`, parallel filter counts, and so on) built on core.async threads. Parallelism pays off when there's enough work per row — a plain scalar sum is memory-bandwidth-bound and runs as fast single-threaded.

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

`read-csv` takes the CSV content as a string or `Reader` (not a file path):

```clojure
(require '[flatiron.io :as io])
(require '[clojure.java.io :as jio])

(with-open [r (jio/reader "data/trades.csv")]
  (let [table (io/read-csv r)]
    (select table :Symbol (sum :Qty))))
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

- **vs `tech.ml.dataset`** — TMD is more feature-rich (date handling, statistical functions, interop with many formats, full I/O ecosystem). Flatiron is smaller — its only dependency beyond Clojure itself is core.async — and focuses on raw speed for a narrower set of operations.

- **vs embedded databases (H2, SQLite)** — Databases give you SQL, transactions, and persistence. Flatiron gives you in-process data you can manipulate directly from Clojure without going through JDBC. If you already have data in Clojure data structures and just need fast analytics, Flatiron is less ceremony. It's also a graph engine: build a CSR graph from two columns and run BFS, Dijkstra, PageRank, or connected components on the same data you're aggregating — in a SQL database that means recursive CTEs at best, or exporting to a separate graph library.

## Benchmarks vs Rayforce (C)

`bench/flatiron/rayforce_bench.clj` runs the same queries through Flatiron and
through [Rayforce](https://github.com/RayforceDB/rayforce), the C17 engine
Flatiron reimplements. Both engines read the same generated dataset; Rayforce
is timed with its `timeit` builtin, Flatiron with `System/nanoTime`, minimum
of 10 runs after warmup.

```
clojure -M:bench -m flatiron.rayforce-bench [path-to-rayforce-binary]
```

Results on an Apple M1 Max (JDK 26, Rayforce `make release`), 1M rows.
The *flatiron* column is the single-threaded path; *flatiron par8* is the
parallel path run with `:n-threads 8` (`parallel-group-by` for the group-by
queries, `parallel-i64-sum` for the scalar sum). Rayforce parallelizes
internally with its own worker pool.

| query                            | rayforce (C) | flatiron | flatiron par8 |
|----------------------------------|-------------:|---------:|--------------:|
| group-by Sym (100 groups), sum   |      0.99 ms | 21.5 ms  |       6.7 ms  |
| group-by Sym, sum+count+avg      |      1.34 ms | 23.9 ms  |      11.5 ms  |
| where Qty>500, group-by Sym sum  |      0.45 ms | 14.8 ms  |       7.2 ms  |
| group-by Id (100K groups), sum   |      2.83 ms | 33.2 ms  |       8.4 ms  |
| scalar sum, 1M i64               |      0.05 ms |  0.7 ms  |       1.0 ms  |

The C implementation is 3–10x faster than Flatiron's parallel path (it uses
SIMD kernels, a custom allocator, and saturates memory bandwidth on scans).
Flatiron's goal is to stay within an order of magnitude of C while remaining
pure Clojure. The scalar sum is the cautionary row: it's memory-bandwidth
bound, so the parallel version loses to the single-threaded loop — thread
dispatch costs more than it saves.

## Acknowledgments

Flatiron is a Clojure reimplementation of ideas from [Rayforce](https://github.com/RayforceDB/rayforce), a SIMD-accelerated columnar analytics and graph engine written in C17. The morsel-driven execution model, the 1024-element batch size, the CSR graph layout with its BFS, DFS, Dijkstra, and PageRank algorithms, and the SQL-like surface all follow Rayforce's design. The wyhash hashing in `flatiron.hash` is ported from Rayforce's `src/ops/hash.h`, and the DSL borrows from its Rayfall query language. Rayforce is MIT licensed.

## License

MIT
