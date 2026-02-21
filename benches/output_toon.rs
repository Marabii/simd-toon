#[macro_use]
extern crate criterion;

#[cfg(feature = "jemallocator")]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use core::time::Duration;
use criterion::{BatchSize, Criterion, Throughput, criterion_group};
use simd_json::Buffers;
use std::fs::File;
use std::io::Read;

const OUTPUT_TOON_PATH: &str =
    "/home/hamza/Desktop/Coding/Projects/simdtoon/simd-toon/data/twitter.toon";

fn bench_output_toon(c: &mut Criterion) {
    let core_ids = core_affinity::get_core_ids().unwrap();
    core_affinity::set_for_current(core_ids[0]);

    let mut vec = Vec::new();
    File::open(OUTPUT_TOON_PATH)
        .expect("Could not open output.toon")
        .read_to_end(&mut vec)
        .expect("Could not read output.toon");

    let mut group = c.benchmark_group("output_toon");
    group.throughput(Throughput::Bytes(vec.len() as u64));
    group
        .warm_up_time(Duration::from_secs(10))
        .measurement_time(Duration::from_secs(60));

    let mut buffers = Buffers::new(vec.len());

    group.bench_with_input("simd_json::to_tape_with_buffers", &vec, |b, data| {
        b.iter_batched(
            || data.clone(),
            |mut bytes| drop(simd_json::to_tape_with_buffers(&mut bytes, &mut buffers).unwrap()),
            BatchSize::LargeInput,
        )
    });

    group.finish();
}

criterion_group!(benches, bench_output_toon);
criterion_main!(benches);
