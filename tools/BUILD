load("@io_bazel_rules_scala//scala:scala_toolchain.bzl", "scala_toolchain")
load("@io_bazel_rules_scala//scala:providers.bzl", "declare_deps_provider")

scala_toolchain(
    name = "tools_scala_impl",
    dep_providers = [
        ":cp_scala_compile",
        ":cp_scala_lib",
        ":cp_scala_macro",
        ":cp_scala_xml",
#        ":cp_scala_parsers",
    ],
    #    scalacopts = ["-Ywarn-unused"],
    unused_dependency_checker_mode = "off",
    visibility = ["//visibility:public"],
)

toolchain(
    name = "tools_scala",
    toolchain = "tools_scala_impl",
    toolchain_type = "@io_bazel_rules_scala//scala:toolchain_type",
    visibility = ["//visibility:public"],
)

declare_deps_provider(
    name = "cp_scala_compile",
    deps_id = "scala_compile_classpath",
    visibility = ["//visibility:public"],
    deps = [
        "@third_party//3rdparty/jvm/org/scala_lang:scala_compiler",
        "@third_party//3rdparty/jvm/org/scala_lang:scala_library",
        "@third_party//3rdparty/jvm/org/scala_lang:scala_reflect",
    ],
)

declare_deps_provider(
    name = "cp_scala_lib",
    deps_id = "scala_library_classpath",
    deps = [
        "@third_party//3rdparty/jvm/org/scala_lang:scala_library",
        "@third_party//3rdparty/jvm/org/scala_lang:scala_reflect",
    ],
)

declare_deps_provider(
    name = "cp_scala_macro",
    deps_id = "scala_macro_classpath",
    deps = [
        "@third_party//3rdparty/jvm/org/scala_lang:scala_library",
        "@third_party//3rdparty/jvm/org/scala_lang:scala_reflect",
    ],
)

declare_deps_provider(
    name = "cp_scala_xml",
    deps_id = "scala_xml",
    deps = ["@third_party//3rdparty/jvm/org/scala_lang/modules:scala_xml"],
)

#declare_deps_provider(
#    name = "cp_scala_parsers",
#    deps_id = "parser_combinators",
#    deps = ["@third_party//3rdparty/jvm/org/scala_lang/modules:scala_parser_combinators_2_13"],
#)
