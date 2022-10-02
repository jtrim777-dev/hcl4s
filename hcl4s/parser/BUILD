scala_library(
    name = "lib",
    srcs = glob(["*.scala"]),
    deps = [
        "//hcl4s/lang:lib",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ],
    exports = [
        "//hcl4s/lang:lib",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ],
    visibility = ["//hcl4s:__subpackages__"]
)