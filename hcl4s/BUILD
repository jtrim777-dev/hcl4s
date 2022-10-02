scala_library(
  name = "lib",
  srcs = glob(["*.scala"]),
  exports = [
    "//hcl4s/eval:lib",
    "//hcl4s/parser:lib",
    "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
  ],
  deps = [
    "//hcl4s/eval:lib",
    "//hcl4s/parser:lib",
    "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
  ]
)

scala_binary(
    name = "hcl4s-test",
    deps = [":lib"],
    main_class = "dev.jtrim777.hcl4s.HCLParser"
)
