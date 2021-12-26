enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
enableFeaturePreview("VERSION_CATALOGS")
rootProject.name = "darklaf"

pluginManagement {
    plugins {
        fun String.v() = extra["$this.version"].toString()
        fun idv(id: String, key: String = id) = id(id) version key.v()
        idv("com.github.autostyle")
        idv("com.github.vlsi.crlf", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.gradle-extensions", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.license-gather", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.stage-vote-release", "com.github.vlsi.vlsi-release-plugins")
        idv("org.ajoberstar.grgit")
        idv("net.ltgt.errorprone")
    }
}

dependencyResolutionManagement {
    versionCatalogs {
        fun String.v() = extra["$this.version"].toString()
        fun VersionCatalogBuilder.versionId(id: String) = version(id, id.v())

        create("libs") {
            versionId("svgSalamander")
            versionId("swingDsl")
            versionId("swingx")
            versionId("javaxAnnotations")
            versionId("nullabilityAnnotations")

            alias("svgSalamander").to("com.formdev", "svgSalamander")
                .versionRef("svgSalamander")

            alias("swingDslLafSupport").to("com.github.weisj", "swing-extensions-laf-support")
                .versionRef("swingDsl")
            alias("visualPaddings").to("com.github.weisj", "swing-extensions-visual-padding")
                .versionRef("swingDsl")
            alias("swingx").to("org.swinglabs", "swingx")
                .versionRef("swingx")

            alias("javaxAnnotations").to("javax.annotation", "javax.annotation-api")
                .versionRef("javaxAnnotations")
            alias("nullabilityAnnotations").to("org.jetbrains", "annotations")
                .versionRef("nullabilityAnnotations")
        }
        create("macOsFrameworks") {
            versionId("javaNativeFoundation")
            versionId("macOSFramework")

            alias("javaNativeFoundation").to("com.github.weisj", "java-native-foundation")
                .versionRef("javaNativeFoundation")
            alias("appKit").to("dev.nokee.framework", "AppKit")
                .versionRef("macOSFramework")
            alias("cocoa").to("dev.nokee.framework", "Cocoa")
                .versionRef("macOSFramework")
        }
        create("testLibs") {
            versionId("junit")
            versionId("miglayout")
            versionId("lGoodDatePicker")
            versionId("rsyntaxtextarea")
            versionId("swingDsl.inspector")
            versionId("jna")

            alias("junit-api").to("org.junit.jupiter", "junit-jupiter-api")
                .versionRef("junit")
            alias("junit-engine").to("org.junit.jupiter", "junit-jupiter-engine")
                .versionRef("junit")

            alias("miglayout-core").to("com.miglayout", "miglayout-core")
                .versionRef("miglayout")
            alias("miglayout-swing").to("com.miglayout", "miglayout-swing")
                .versionRef("miglayout")
            bundle("miglayout", listOf("miglayout-core", "miglayout-swing"))

            alias("lGoodDatePicker").to("com.github.lgooddatepicker", "LGoodDatePicker")
                .versionRef("lGoodDatePicker")
            alias("rsyntaxtextarea").to("com.fifesoft", "rsyntaxtextarea")
                .versionRef("rsyntaxtextarea")
            alias("swingDslInspector").to("com.github.weisj", "swing-extensions-inspector")
                .versionRef("swingDsl.inspector")

            alias("jna").to("net.java.dev.jna", "jna")
                .versionRef("jna")
        }
        create("toolLibs") {
            versionId("errorprone")
            versionId("errorprone.compiler")
            versionId("guava")
            versionId("autoservice")

            alias("errorprone-core").to("com.google.errorprone", "error_prone_core")
                .versionRef("errorprone")
            alias("errorprone-annotations").to("com.google.errorprone", "error_prone_annotations")
                .versionRef("errorprone")
            alias("errorprone-guava").to("com.google.guava", "guava-beta-checker")
                .versionRef("errorprone")
            alias("errorprone-javac").to("com.google.errorprone", "javac")
                .versionRef("errorprone")

            alias("autoservice-annotations").to("com.google.auto.service", "auto-service-annotations")
                .versionRef("autoservice")
            alias("autoservice-processor").to("com.google.auto.service", "auto-service")
                .versionRef("autoservice")
        }
    }
}

include(
    "annotations",
    "annotations-processor",
    "native-utils",
    "core",
    "compatibility",
    "iconset",
    "theme",
    "property-loader",
    "utils",
    "platform-base",
    "windows",
    "macos"
)

for (p in rootProject.children) {
    if (p.children.isEmpty()) {
        // Rename leaf projects only
        // E.g. we don't expect to publish examples as a Maven module
        p.name = "darklaf-" + p.name
    }
}
