plugins {
    `java-platform`
}

val String.v: String get() = rootProject.extra["$this.version"] as String

// Note: Gradle allows to declare dependency on "bom" as "api",
// and it makes the constraints to be transitively visible
// However Maven can't express that, so the approach is to use Gradle resolution
// and generate pom files with resolved versions
// See https://github.com/gradle/gradle/issues/9866

fun DependencyConstraintHandlerScope.apiv(
    notation: String,
    versionProp: String = notation.substringAfterLast(':')
) =
    "api"("$notation:${versionProp.v}")

fun DependencyConstraintHandlerScope.runtimev(
    notation: String,
    versionProp: String = notation.substringAfterLast(':')
) =
    "runtimeOnly"("$notation:${versionProp.v}")

dependencies {
    // Parenthesis are needed here: https://github.com/gradle/gradle/issues/9248
    (constraints) {
        // api means "the dependency is for both compilation and runtime"
        // runtime means "the dependency is only for runtime, not for compilation"
        // In other words, marking dependency as "runtime" would avoid accidental
        // dependency on it during compilation
        apiv("net.java.dev.jna:jna")
        apiv("org.swinglabs:jxlayer")
        apiv("org.swinglabs:swingx")
        apiv("com.formdev:svgSalamander")
        apiv("com.fifesoft:rsyntaxtextarea")
        apiv("com.miglayout:miglayout-core", "miglayout")
        apiv("com.miglayout:miglayout-swing", "miglayout")
        apiv("org.junit.jupiter:junit-jupiter-api", "junit")
        apiv("org.junit.jupiter:junit-jupiter-engine", "junit")
        apiv("com.google.auto.service:auto-service-annotations", "auto-service")
        apiv("com.google.auto.service:auto-service", "auto-service")
        apiv("com.github.lgooddatepicker:LGoodDatePicker")
        apiv("com.github.weisj:swing-dsl-laf-support", "swing-dsl")
    }
}
