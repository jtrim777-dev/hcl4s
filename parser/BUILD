scala_library(
    name = "lib",
    srcs = glob(["src/main/scala/*.scala"]),
    visibility = ["//visibility:public"],
    exports = [
        "//core:lang-lib",
        "//core:util-lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:fastparse",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
    deps = [
        "//core:lang-lib",
        "//core:util-lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:fastparse",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
)

scala_binary(
    name = "test",
    srcs = ["src/test/scala/TestParse.scala"],
    deps = [
        "//parser:lib",
        "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
    ],
    main_class = "dev.jtrim777.hcl4s.parser.TestParse",
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
