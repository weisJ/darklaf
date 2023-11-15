import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings
import com.github.vlsi.gradle.properties.dsl.props
import com.github.vlsi.gradle.properties.dsl.stringProperty
import com.github.vlsi.gradle.properties.dsl.toBool
import com.github.vlsi.gradle.publishing.dsl.simplifyXml
import com.github.vlsi.gradle.publishing.dsl.versionFromResolution
import net.ltgt.gradle.errorprone.errorprone
import org.gradle.api.tasks.testing.logging.TestExceptionFormat
import java.util.*

plugins {
    idea
    id("com.diffplug.spotless")
    id("com.github.vlsi.crlf")
    id("com.github.vlsi.gradle-extensions")
    id("com.github.vlsi.stage-vote-release")
    id("org.ajoberstar.grgit")
    id("net.ltgt.errorprone") apply false
}

val skipJavadoc by props()
val enableMavenLocal by props(false)
val enableGradleMetadata by props()
val enableErrorProne by props()
val skipSpotless by props(false)
val isRelease = project.stringProperty("release").toBool()
val snapshotName by props("")

if (isRelease && !JavaVersion.current().isJava9Compatible) {
    throw GradleException("Java 9 compatible compiler is needed for release builds")
}

val String.v: String get() = rootProject.extra["$this.version"] as String
val projectVersion = "darklaf".v

val snapshotIdentifier = if (!isRelease && snapshotName.isNotEmpty()) "-$snapshotName" else ""

releaseParams {
    tlp.set("darklaf")
    organizationName.set("weisJ")
    componentName.set("darklaf")
    prefixForProperties.set("gh")
    svnDistEnabled.set(false)
    sitePreviewEnabled.set(false)
    release.set(isRelease)
    if (!isRelease) {
        rcTag.set("v$projectVersion$snapshotIdentifier$snapshotSuffix")
    }
    nexus {
        mavenCentral()
    }
    voteText.set {
        """
        ${it.componentName} v${it.version}-rc${it.rc} is ready for preview.

        Git SHA: ${it.gitSha}
        Staging repository: ${it.nexusRepositoryUri}
        """.trimIndent()
    }
}

tasks.closeRepository.configure { enabled = isRelease }

val buildVersion = "$projectVersion$snapshotIdentifier${releaseParams.snapshotSuffix}"
println("Building: Darklaf $buildVersion")
println("     JDK: " + System.getProperty("java.home"))
println("  Gradle: " + gradle.gradleVersion)

allprojects {
    group = "com.github.weisj"
    version = buildVersion

    repositories {
        if (enableMavenLocal) {
            mavenLocal()
        }
        if (!isRelease) {
            maven { url = uri("https://oss.sonatype.org/content/repositories/snapshots/") }
        }
        mavenCentral()
    }

    configurations.all {
        resolutionStrategy.cacheChangingModulesFor(0, "seconds")
    }

    val githubAccessToken by props("")

    plugins.withType<UsePrebuiltBinariesWhenUnbuildablePlugin> {
        val failIfLibraryMissing by props(false)
        prebuiltBinaries {
            prebuiltLibrariesFolder = "pre-build-libraries"
            failIfLibraryIsMissing = failIfLibraryMissing
            github(
                user = "weisj",
                repository = "darklaf",
                workflow = "libs.yml"
            ) {
                val currentBranch = System.getenv("GITHUB_HEAD_REF") ?: grgit.branch.current()?.name
                branches = listOfNotNull(currentBranch, "master", "v$projectVersion", projectVersion)
                accessToken = githubAccessToken
                manualDownloadUrl =
                    "https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22+is%3Asuccess+branch%3Amaster"
                timeout = 50000
            }
        }
    }

    if (!skipSpotless) {
        apply(plugin = "com.diffplug.spotless")
        spotless {
            val spotlessRatchet by props(default = true)
            if (spotlessRatchet) {
                ratchetFrom("origin/master")
            }
            kotlinGradle {
                ktlint("ktlint".v)
            }
            format("markdown") {
                target("**/*.md")
                endWithNewline()
                trimTrailingWhitespace()
            }
            format("svg") {
                target("**/*.svg")
                endWithNewline()
                trimTrailingWhitespace()
                eclipseWtp(com.diffplug.spotless.extra.wtp.EclipseWtpFormatterStep.XML)
            }
            plugins.withType<dev.nokee.platform.jni.internal.plugins.JniLibraryPlugin>().configureEach {
                cpp {
                    target("**/*.cpp", "**/*.h")
                    targetExclude("**/objcpp/**")
                    endWithNewline()
                    trimTrailingWhitespace()
                    eclipseCdt().configFile("${project.rootDir}/config/darklaf_cpp.eclipseformat.xml")
                    licenseHeaderFile("${project.rootDir}/config/LICENSE_HEADER_JAVA.txt")
                }
            }
            plugins.withType<JavaPlugin>().configureEach {
                format("properties") {
                    target("**/*.properties")
                    targetExclude("*/build/")
                    licenseHeaderFile("${project.rootDir}/config/LICENSE_HEADER_PROPERTIES.txt", "[^#]")
                }
                java {
                    importOrder("java", "javax", "org", "com")
                    removeUnusedImports()
                    endWithNewline()
                    trimTrailingWhitespace()
                    eclipse().configFile("${project.rootDir}/config/darklaf_java.eclipseformat.xml")
                    licenseHeaderFile("${project.rootDir}/config/LICENSE_HEADER_JAVA.txt")
                }
            }
        }
    }

    tasks.withType<AbstractArchiveTask>().configureEach {
        // Ensure builds are reproducible
        isPreserveFileTimestamps = false
        isReproducibleFileOrder = true
        dirMode = "775".toInt(8)
        fileMode = "664".toInt(8)
    }

    if (!enableGradleMetadata) {
        tasks.withType<GenerateModuleMetadata> {
            enabled = false
        }
    }

    plugins.withType<JavaPlugin> {
        configure<JavaPluginExtension> {
            sourceCompatibility = JavaVersion.VERSION_17
            targetCompatibility = JavaVersion.VERSION_17
            withSourcesJar()
            if (!skipJavadoc && isRelease) {
                withJavadocJar()
            }
        }
        apply(plugin = "maven-publish")

        val useInMemoryKey by props()
        if (useInMemoryKey) {
            apply(plugin = "signing")

            configure<SigningExtension> {
                useInMemoryPgpKeys(
                    project.stringProperty("signing.inMemoryKey")?.replace("#", "\n"),
                    project.stringProperty("signing.password")
                )
            }
        }

        if (enableErrorProne) {
            apply(plugin = "net.ltgt.errorprone")
            dependencies {
                "errorprone"(toolLibs.errorprone.core)
                "annotationProcessor"(toolLibs.errorprone.guava)
                if (!JavaVersion.current().isJava9Compatible) {
                    "errorproneJavac"(toolLibs.errorprone.javac)
                }
            }
            tasks.withType<JavaCompile>().configureEach {
                options.compilerArgs.addAll(listOf("-Xmaxerrs", "10000", "-Xmaxwarns", "10000"))
                if (props.bool("Werror", false)) {
                    options.compilerArgs.add("-Werror")
                }
                options.errorprone {
                    errorproneArgs.add("-XepExcludedPaths:.*/javacc/.*")
                    disableWarningsInGeneratedCode.set(true)
                    disable(
                        "StringSplitter",
                        "InlineMeSuggester",
                        "MissingSummary"
                    )
                }
            }
        }

        tasks {
            withType<Test>().configureEach {
                testLogging {
                    showStandardStreams = true
                    showExceptions = true
                    showStackTraces = true
                    exceptionFormat = TestExceptionFormat.FULL
                }
            }

            withType<JavaCompile>().configureEach {
                options.encoding = "UTF-8"
            }

            withType<ProcessResources>().configureEach {
                from(source) {
                    include("**/*.properties")
                    filteringCharset = "UTF-8"
                    duplicatesStrategy = DuplicatesStrategy.INCLUDE
                    // apply native2ascii conversion since Java 8 expects properties to have ascii symbols only
                    filter(org.apache.tools.ant.filters.EscapeUnicode::class)
                }
            }

            withType<Jar>().configureEach {
                manifest {
                    attributes["Bundle-License"] = "MIT"
                    attributes["Implementation-Title"] = project.name
                    attributes["Implementation-Version"] = project.version
                    attributes["Specification-Vendor"] = "Darklaf"
                    attributes["Specification-Version"] = project.version
                    attributes["Specification-Title"] = "Darklaf"
                    attributes["Implementation-Vendor"] = "Darklaf"
                    attributes["Implementation-Vendor-Id"] = "com.github.weisj"
                }

                CrLfSpec(LineEndings.LF).run {
                    into("META-INF") {
                        filteringCharset = "UTF-8"
                        duplicatesStrategy = DuplicatesStrategy.EXCLUDE
                        // This includes either project-specific license or a default one
                        if (file("$projectDir/LICENSE").exists()) {
                            textFrom("$projectDir/LICENSE")
                        } else {
                            textFrom("$rootDir/LICENSE")
                        }
                    }
                }
            }

            withType<Javadoc>().configureEach {
                (options as StandardJavadocDocletOptions).apply {
                    quiet()
                    locale = "en"
                    docEncoding = "UTF-8"
                    charSet = "UTF-8"
                    encoding = "UTF-8"
                    docTitle = "Darklaf ${project.name} API"
                    windowTitle = "Darklaf ${project.name} API"
                    header = "<b>Darklaf</b>"
                    addBooleanOption("Xdoclint:none", true)
                    addStringOption("source", "8")
                    if (JavaVersion.current().isJava9Compatible) {
                        addBooleanOption("html5", true)
                        links("https://docs.oracle.com/javase/9/docs/api/")
                    } else {
                        links("https://docs.oracle.com/javase/8/docs/api/")
                    }
                }
            }
        }

        configure<PublishingExtension> {
            if (project.path == ":") {
                // Skip the root project
                return@configure
            }

            publications {
                create<MavenPublication>(project.name) {
                    artifactId = "${project.name}$snapshotIdentifier"
                    version = buildVersion
                    description = project.description
                    from(project.components["java"])
                }
                withType<MavenPublication> {
                    // Use the resolved versions in pom.xml
                    // Gradle might have different resolution rules, so we set the versions
                    // that were used in Gradle build/test.
                    versionFromResolution()
                    pom {
                        simplifyXml()

                        description.set(
                            project.description
                                ?: "A themeable Look and Feel for java swing"
                        )
                        name.set(
                            (project.findProperty("artifact.name") as? String)
                                ?: project.name
                                    .replaceFirstChar {
                                        if (it.isLowerCase()) it.titlecase(Locale.getDefault())
                                        else it.toString()
                                    }
                                    .replace("-", " ")
                        )
                        url.set("https://github.com/weisJ/darklaf")
                        organization {
                            name.set("com.github.weisj")
                            url.set("https://github.com/weisj")
                        }
                        issueManagement {
                            system.set("GitHub")
                            url.set("https://github.com/weisJ/darklaf/issues")
                        }
                        licenses {
                            license {
                                name.set("MIT")
                                url.set("https://github.com/weisJ/darklaf/blob/master/LICENSE")
                                distribution.set("repo")
                            }
                        }
                        scm {
                            url.set("https://github.com/weisJ/darklaf")
                            connection.set("scm:git:git://github.com/weisJ/darklaf.git")
                            developerConnection.set("scm:git:ssh://git@github.com:weisj/darklaf.git")
                        }
                        developers {
                            developer {
                                name.set("Jannis Weis")
                            }
                        }
                    }
                }
            }
        }
    }
}
