enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")
enableFeaturePreview("VERSION_CATALOGS")

pluginManagement {
    plugins {
        fun String.v() = extra["$this.version"].toString()
        fun idv(id: String, key: String = id) = id(id) version key.v()
        idv("com.github.autostyle")
        idv("com.github.vlsi.crlf", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.gradle-extensions", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.license-gather", "com.github.vlsi.vlsi-release-plugins")
        idv("com.github.vlsi.stage-vote-release", "com.github.vlsi.vlsi-release-plugins")
        idv("org.ajoberstar.grgit", "org.ajoberstar.grgit")
    }
}
rootProject.name = "darklaf"

include(
    "annotations",
    "annotations-processor",
    "native-utils",
    "core",
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
