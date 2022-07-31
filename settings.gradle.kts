enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
enableFeaturePreview("VERSION_CATALOGS")
rootProject.name = "darklaf"

pluginManagement {
    plugins {
        fun idv(id: String, key: String = id) = id(id) version extra["$key.version"].toString()
        idv("com.diffplug.spotless")
        idv("com.github.vlsi.crlf", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.gradle-extensions", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.license-gather", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.stage-vote-release", "com.github.vlsi.vlsi-release-plugins")
        idv("org.ajoberstar.grgit")
        idv("net.ltgt.errorprone")
    }
}

plugins {
    id("dev.nokee.nokee-version-management") version("1.0.0")
}

dependencyResolutionManagement {
    versionCatalogs {
        fun VersionCatalogBuilder.idv(name: String, coordinates: String, versionRef: String = name) {
            val parts = coordinates.split(':', limit = 2)
            alias(name).to(parts[0], parts[1]).version(extra["$versionRef.version"].toString())
        }
        class VersionBundle(val bundleName: String, val builder: VersionCatalogBuilder) {
            val libs = mutableListOf<String>()
            fun idv(name: String, coordinates: String, versionRef: String = bundleName) =
                builder.idv("$bundleName-$name".also { libs.add(it) }, coordinates, versionRef)
        }
        fun VersionCatalogBuilder.bundle(name: String, init: VersionBundle.() -> Unit) = VersionBundle(name, this).run {
            init()
            bundle(name, libs)
        }

        create("libs") {
            idv("jsvg", "com.github.weisj:jsvg")
            idv("swingDslLafSupport", "com.github.weisj:swing-extensions-laf-support", "swingDsl")
            idv("visualPaddings", "com.github.weisj:swing-extensions-visual-padding", "swingDsl")
            idv("swingx", "org.swinglabs:swingx")

            idv("javaxAnnotations", "javax.annotation:javax.annotation-api")
            idv("nullabilityAnnotations", "org.jetbrains:annotations")
        }
        create("macOsFrameworks") {
            idv("appKit", "dev.nokee.framework:AppKit", "macOSFramework")
            idv("cocoa", "dev.nokee.framework:Cocoa", "macOSFramework")
        }
        create("testLibs") {
            bundle("junit") {
                idv("api", "org.junit.jupiter:junit-jupiter-api")
                idv("engine", "org.junit.jupiter:junit-jupiter-engine")
            }
            bundle("miglayout") {
                idv("core", "com.miglayout:miglayout-core")
                idv("swing", "com.miglayout:miglayout-swing")
            }
            idv("lGoodDatePicker", "com.github.lgooddatepicker:LGoodDatePicker")
            idv("rsyntaxtextarea", "com.fifesoft:rsyntaxtextarea")
            idv("swingDslInspector", "com.github.weisj:swing-extensions-inspector", "swingDsl.inspector")
            idv("jna", "net.java.dev.jna:jna")
        }
        create("toolLibs") {
            bundle("errorprone") {
                idv("core", "com.google.errorprone:error_prone_core")
                idv("annotations", "com.google.errorprone:error_prone_annotations")
                idv("javac", "com.google.errorprone:javac", "errorprone.compiler")
                idv("guava", "com.google.guava:guava-beta-checker", "guava")
            }
            bundle("autoservice") {
                idv("annotations", "com.google.auto.service:auto-service-annotations")
                idv("processor", "com.google.auto.service:auto-service")
            }
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
    "theme-spec",
    "property-loader",
    "utils",
    "windows",
    "macos",
    "platform-base",
    "platform-decorations",
    "platform-preferences"
)

for (p in rootProject.children) {
    if (p.children.isEmpty()) {
        // Rename leaf projects only
        // E.g. we don't expect to publish examples as a Maven module
        p.name = "darklaf-" + p.name
    }
}
