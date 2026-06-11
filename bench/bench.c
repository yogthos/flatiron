/*
 * Flatiron vs C baseline benchmark.
 * Compile: cc -O3 -march=native -o bench/bench_c bench/bench.c
 * Run:     ./bench/bench_c
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static double now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec * 1e9 + (double)ts.tv_nsec;
}

/* Volatile sink prevents the compiler from optimizing away the loop. */
static volatile long   sink_l;
static volatile double sink_d;

/* ── I64 sum ─────────────────────────────────────────────────────────── */
static void i64_sum(long *arr, long n) {
    long acc = 0;
    for (long i = 0; i < n; i++) acc += arr[i];
    sink_l = acc;
}

/* ── F64 sum ─────────────────────────────────────────────────────────── */
static void f64_sum(double *arr, long n) {
    double acc = 0.0;
    for (long i = 0; i < n; i++) acc += arr[i];
    sink_d = acc;
}

/* ── I64 avg ─────────────────────────────────────────────────────────── */
static void i64_avg(long *arr, long n) {
    double sum = 0.0;
    for (long i = 0; i < n; i++) sum += (double)arr[i];
    sink_d = sum / (double)n;
}

/* ── Main ────────────────────────────────────────────────────────────── */
int main(void) {
    long sizes[] = {100000, 1000000, 5000000};
    int n_sizes = 3;
    long min_iters = 100;

    printf("═══ C Baseline (raw arrays, -O3 -march=native) ═══\n\n");

    for (int s = 0; s < n_sizes; s++) {
        long n = sizes[s];
        printf("--- %ld rows ---\n", n);

        long   *i64_arr = malloc((size_t)n * sizeof(long));
        double *f64_arr = malloc((size_t)n * sizeof(double));
        for (long i = 0; i < n; i++) {
            i64_arr[i] = (long)(rand() % 1000000);
            f64_arr[i] = (double)rand() / (double)RAND_MAX;
        }

        /* I64 sum */
        {
            long iters = min_iters;
            double t0 = now_ns();
            for (long k = 0; k < iters; k++) i64_sum(i64_arr, n);
            double dt = now_ns() - t0;
            printf("  i64-sum:  %8.0f ns/op  (%ld iters, check=%ld)\n",
                   dt / (double)iters, iters, sink_l);
        }

        /* F64 sum */
        {
            long iters = min_iters;
            double t0 = now_ns();
            for (long k = 0; k < iters; k++) f64_sum(f64_arr, n);
            double dt = now_ns() - t0;
            printf("  f64-sum:  %8.0f ns/op  (%ld iters, check=%.0f)\n",
                   dt / (double)iters, iters, sink_d);
        }

        /* I64 avg */
        {
            long iters = min_iters;
            double t0 = now_ns();
            for (long k = 0; k < iters; k++) i64_avg(i64_arr, n);
            double dt = now_ns() - t0;
            printf("  i64-avg:  %8.0f ns/op  (%ld iters, check=%.0f)\n",
                   dt / (double)iters, iters, sink_d);
        }

        free(i64_arr);
        free(f64_arr);
        printf("\n");
    }

    printf("Done.\n");
    return 0;
}
