
scala_library(
    name = "lib",
    srcs = glob(["src/main/scala/**/*.scala"]),
    deps = [
        "//core:lang-lib",
        "//core:model-lib",
        "@third_party//3rdparty/jvm/com/chuusai:shapeless",
    ],
    exports = [
        "//core:lang-lib",
        "//core:model-lib",
        "@third_party//3rdparty/jvm/com/chuusai:shapeless",
    ],
    visibility = ["//visibility:public"],
)