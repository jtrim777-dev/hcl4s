scala_library(
  name = "core-lib",
  srcs = [],
  exports = [
    "//hcl4s/lang:lib",
    "//hcl4s/model:lib",
    "//hcl4s/eval:lib",
  ],
  deps = [
    "//hcl4s/lang:lib",
    "//hcl4s/model:lib",
    "//hcl4s/eval:lib",
  ],
  visibility = ["//visibility:public"],
  tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s-core_2.13:{pom_version}"],
)

scala_library(
  name = "lib",
  srcs = glob(["*.scala"]),
  exports = [
    ":core-lib",
    "//hcl4s/parser:lib",
  ],
  deps = [
    ":core-lib",
    "//hcl4s/parser:lib",
  ],
  visibility = ["//visibility:public"],
  tags = ["maven_coordinates=dev.jtrim777.hcl4s:hcl4s_2.13:{pom_version}"],
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
