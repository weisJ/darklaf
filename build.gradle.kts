import com.github.autostyle.generic.DefaultCopyrightStyle
import com.github.autostyle.gradle.BaseFormatExtension
import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings
import com.github.vlsi.gradle.properties.dsl.props
import com.github.vlsi.gradle.properties.dsl.stringProperty
import com.github.vlsi.gradle.properties.dsl.toBool
import com.github.vlsi.gradle.publishing.dsl.simplifyXml
import com.github.vlsi.gradle.publishing.dsl.versionFromResolution

plugins {
    id("com.github.autostyle")
    id("com.github.vlsi.crlf")
    id("com.github.vlsi.gradle-extensions")
    id("com.github.vlsi.stage-vote-release")
}

val skipJavadoc by props()
val enableMavenLocal by props()
val enableGradleMetadata by props()
val skipAutostyle by props()
val isRelease = project.stringProperty("release").toBool()
val snapshotName by props("")

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

fun BaseFormatExtension.license(addition: String = "") {
    val extra = if (addition.isEmpty()) "" else "\n$addition"
    licenseHeader(File("${project.rootDir}/LICENSE").readText() + extra) {
        filter {
            exclude("**/org/pbjar/jxlayer/*")
        }
        copyrightStyle("bat", DefaultCopyrightStyle.REM)
        copyrightStyle("cmd", DefaultCopyrightStyle.REM)
    }
    trimTrailingWhitespace()
    if (addition.isEmpty()) {
        endWithNewline()
    }
}

fun BaseFormatExtension.configFilter(init: PatternFilterable.() -> Unit) {
    filter {
        // Autostyle does not support gitignore yet https://github.com/autostyle/autostyle/issues/13
        exclude("out/**")
        exclude(".idea/**", ".run/**")
        if (project == rootProject) {
            exclude("gradlew*", "gradle/**")
        } else {
            exclude("bin/**")
        }
        init()
    }
}

allprojects {
    group = "com.github.weisj"
    version = buildVersion

    repositories {
        if (enableMavenLocal) {
            mavenLocal()
        }
        mavenCentral()
    }

    val githubAccessToken by props("")

    plugins.withType<UsePrebuiltBinariesWhenUnbuildablePlugin> {
        prebuildBinaries {
            prebuildLibrariesFolder = "pre-build-libraries"
            missingLibraryIsFailure = false
            github {
                user = "weisj"
                repository = "darklaf"
                workflow = "libs.yml"
                branches = listOf("master", "v$projectVersion", projectVersion)
                accessToken = githubAccessToken
                manualDownloadUrl =
                    "https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22+is%3Asuccess+branch%3Amaster"
                timeout = 50000
            }
        }
    }

    if (!skipAutostyle) {
        apply(plugin = "com.github.autostyle")
        autostyle {
            kotlinGradle {
                ktlint()
            }
            format("properties") {
                configFilter {
                    include("**/*.properties")
                    exclude("**/gradle.properties")
                }
                license("\nsuppress inspection \"UnusedProperty\" for whole file")
            }
            format("configs") {
                configFilter {
                    include("**/*.sh", "**/*.bsh", "**/*.cmd", "**/*.bat")
                    include("**/*.xsd", "**/*.xsl", "**/*.xml")
                    exclude("**/*.eclipseformat.xml")
                }
                license()
            }
            format("markdown") {
                filter.include("**/*.md")
                endWithNewline()
            }
            cpp {
                trimTrailingWhitespace()
                endWithNewline()
                license()
                eclipse {
                    configFile("${project.rootDir}/darklaf_cpp.eclipseformat.xml")
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

    plugins.withType<JavaLibraryPlugin> {
        dependencies {
            "api"(platform(project(":darklaf-dependencies-bom")))
            "annotationProcessor"(platform(project(":darklaf-dependencies-bom")))
        }
    }

    if (!enableGradleMetadata) {
        tasks.withType<GenerateModuleMetadata> {
            enabled = false
        }
    }

    plugins.withType<JavaPlugin> {
        configure<JavaPluginExtension> {
            sourceCompatibility = JavaVersion.VERSION_1_8
            targetCompatibility = JavaVersion.VERSION_1_8
            withSourcesJar()
            if (!skipJavadoc && isRelease) {
                withJavadocJar()
            }
        }

        if (!skipAutostyle) {
            autostyle {
                java {
                    importOrder("java", "javax", "org", "com")
                    removeUnusedImports()
                    license()
                    eclipse {
                        configFile("${project.rootDir}/darklaf_java.eclipseformat.xml")
                    }
                }
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

        tasks {
            withType<JavaCompile>().configureEach {
                options.encoding = "UTF-8"
            }

            withType<ProcessResources>().configureEach {
                from(source) {
                    include("**/*.properties")
                    filteringCharset = "UTF-8"
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
                    // -add-exports requires target 9
                    // The library is built with target=1.8, so add-exports
                    if (project.the<JavaPluginExtension>().targetCompatibility.isJava9Compatible) {
                        addStringOption("-add-exports", "java.desktop/sun.swing=ALL-UNNAMED")
                        addStringOption("-add-exports", "java.desktop/sun.awt=ALL-UNNAMED")
                        addStringOption("-add-exports", "java.desktop/com.sun.java.swing=ALL-UNNAMED")
                        addStringOption("-add-exports", "java.desktop/sun.awt.shell=ALL-UNNAMED")
                    }
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
            if (project.path.startsWith(":darklaf-dependencies-bom") ||
                project.path == ":"
            ) {
                // We don't it to Central for now
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
                                ?: project.name.capitalize().replace("-", " ")
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
