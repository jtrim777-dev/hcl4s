load("@rules_scala//rules/register_toolchain.bzl")

compiler_classpath_2_13 = [
    "@maven//:org_scala_lang_scala_compiler",
    "@maven//:org_scala_lang_scala_library",
    "@maven//:org_scala_lang_scala_reflect",
]
runtime_classpath_2_13 = ["@maven//:org_scala_lang_scala_library",]

register_bootstrap_toolchain(
		name = "scala_2_13_8",
		compiler_classpath = compiler_classpath_2_13,
		runtime_classpath = runtime_classpath_2_13,
		version = "2.13.8",
		visibility = ["//visibility:public"],
)
