#[macro_use]
extern crate log;

use std::env;
use std::path::{Path, PathBuf};

#[cfg(feature = "gen")]
fn gen_binding(out_file: &Path) {
    let libpcap = pkg_config::probe_library("libpcap").expect("libpcap installed");

    warn!("found libpcap: {:?}", libpcap);

    let pcap_header = libpcap.include_paths.first().unwrap().join("pcap.h");

    bindgen::Builder::default()
        .header(pcap_header.to_str().unwrap())
        .whitelist_type("bpf_.*")
        .whitelist_function("pcap_compile|bpf_.*")
        .whitelist_var("BPF_.*")
        .clang_args(
            libpcap
                .include_paths
                .into_iter()
                .map(|inc| format!("-I{}", inc.to_str().unwrap()))
                .chain(libpcap.defines.into_iter().map(|(key, value)| {
                    if let Some(value) = value {
                        format!("-D{}={}", key, value)
                    } else {
                        format!("-D{}", key)
                    }
                })),
        )
        .rustfmt_bindings(true)
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file(out_file)
        .expect("Couldn't write bindings!");
}

#[cfg(not(feature = "gen"))]
fn gen_binding(out_file: &Path) {
    info!("copy pre-generated binding file to {:?}", out_file);

    ::std::fs::copy("src/raw.rs", out_file).expect("pre-generated binding file");
}

fn main() {
    pretty_env_logger::init();

    let out_dir: PathBuf = env::var("OUT_DIR").unwrap().into();
    let binding_file = out_dir.join("raw.rs");

    gen_binding(&binding_file);
}