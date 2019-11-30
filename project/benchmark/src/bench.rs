use cpython::{NoArgs, ObjectProtocol, PyResult, Python};
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::time::{Duration, Instant};

pub fn run_benchmark<T: Debug + Copy + Hash + Ord, U: Debug + PartialEq>(
    reps: usize,
    range: impl Iterator<Item = T>,
    benchees: &[(impl ToString, &dyn Fn(T) -> U)],
) -> BenchResult<T> {
    let mut time_measurements = BTreeMap::new();
    for param in range {
        let mut times = vec![];
        for rep in 0..reps {
            let mut f_times = vec![];
            let mut results = vec![];
            for (_, f) in benchees {
                let start = Instant::now();
                let res = f(param);
                let time_taken = start.elapsed();
                f_times.push(time_taken);
                results.push(res);
            }

            for w in results.windows(2) {
                if w[0] != w[1] {
                    panic!(
                        "result mismatch ({:?} != {:?}) in repetetion {} for parameter {:?}",
                        w[0], w[1], rep, param
                    );
                }
            }

            times.push(f_times);
        }
        time_measurements.insert(param, times);
    }

    BenchResult::new(
        time_measurements,
        benchees.iter().map(|(n, _)| n.to_string()).collect(),
    )
}

pub struct BenchResult<T> {
    measurements: HashMap<String, BTreeMap<T, Vec<Duration>>>,
}

impl<T: Copy + Ord> BenchResult<T> {
    pub fn new(data: BTreeMap<T, Vec<Vec<Duration>>>, names: Vec<String>) -> Self {
        let mut measurements: HashMap<_, BTreeMap<T, Vec<_>>> =
            names.iter().map(|n| (n.clone(), BTreeMap::new())).collect();

        for (param, runs) in data {
            for rep in runs {
                for (name, dur) in names.iter().zip(rep) {
                    measurements
                        .get_mut(name)
                        .expect("missing measurement entry")
                        .entry(param)
                        .or_insert(vec![])
                        .push(dur);
                }
            }
        }

        BenchResult { measurements }
    }
}

pub trait Plotable {
    fn as_f64(&self) -> f64;
}

impl Plotable for f64 {
    fn as_f64(&self) -> f64 {
        *self
    }
}

impl Plotable for i64 {
    fn as_f64(&self) -> f64 {
        *self as f64
    }
}

impl<T: Copy + Ord + Plotable> BenchResult<T> {
    pub fn plot(&self) -> PyResult<()> {
        let gil = Python::acquire_gil();
        let py = gil.python();

        let plt = py.import("matplotlib.pyplot")?;

        // I'm sure a macro could make this nicer...

        for (name, m) in &self.measurements {
            let mut x: Vec<f64> = vec![];
            let mut y: Vec<f64> = vec![];
            let mut s: Vec<f64> = vec![];
            for (p, r) in m {
                x.push(p.as_f64());
                let mean = r.iter().map(|d| d.as_secs_f64()).sum::<f64>() / r.len() as f64;
                let std = r
                    .iter()
                    .map(|d| (d.as_secs_f64() - mean).powf(2.0).sqrt())
                    .sum::<f64>()
                    / r.len() as f64;
                y.push(mean);
                s.push(std);
            }
            plt.get(py, "errorbar")?.call(py, (x, y, s), None)?;
        }
        plt.get(py, "legend")?
            .call(py, (self.measurements.keys().collect::<Vec<_>>(),), None)?;
        plt.get(py, "show")?.call(py, NoArgs, None)?;

        Ok(())
    }
}
