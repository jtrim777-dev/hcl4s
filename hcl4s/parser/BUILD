scala_library(
    name = "lib",
    srcs = glob(["*.scala"]),
    visibility = ["//visibility:public"],
    exports = [
        "//hcl4s/lang:lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:fastparse",
    ],
    deps = [
        "//hcl4s/lang:lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:fastparse",
    ],
)

scala_binary(
    name = "repl",
    srcs = [],
    main_class = "ammonite.Main",
    deps = [
        ":lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:ammonite_2_13_8",
    ],
)
