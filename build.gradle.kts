import com.github.autostyle.generic.DefaultCopyrightStyle
import com.github.autostyle.gradle.BaseFormatExtension
import com.github.vlsi.gradle.crlf.CrLfSpec
import com.github.vlsi.gradle.crlf.LineEndings
import com.github.vlsi.gradle.properties.dsl.props
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

val String.v: String get() = rootProject.extra["$this.version"] as String

val buildVersion = "darklaf".v + releaseParams.snapshotSuffix
println("Building: Darklaf $buildVersion")
println("     JDK: " + System.getProperty("java.home"))

releaseParams {
    tlp.set("darklaf")
    organizationName.set("weisJ")
    componentName.set("darklaf")
    prefixForProperties.set("gh")
    svnDistEnabled.set(false)
    sitePreviewEnabled.set(false)
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
                    include("**/*.yml")
                    include("**/*.xsd", "**/*.xsl", "**/*.xml")
                }
                license()
            }
            format("markdown") {
                filter.include("**/*.md")
                endWithNewline()
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
            // cpp-library is not compatible with java-library
            // they both use api and implementation configurations
            val bom = platform(project(":darklaf-dependencies-bom"))
            if (!plugins.hasPlugin("cpp-library")) {
                "api"(bom)
            } else {
                // cpp-library does not know these configurations, so they are for Java
                "compileOnly"(bom)
                "runtimeOnly"(bom)
            }
        }
    }

    plugins.withId("cpp-library") {
        listOf(AbstractPublishToMaven::class, GenerateMavenPom::class, GenerateModuleMetadata::class)
            .forEach { type ->
                tasks.withType(type)
                    .matching {
                        it.name.startsWith("publishMain") ||
                                it.name.startsWith("signMain") ||
                                it.name.startsWith("generatePomFileForMain") ||
                                it.name.startsWith("generateMetadataFileForMain")
                    }
                    .configureEach {
                        // We don't need to publish CPP artifacts (e.g. header files)
                        enabled = false
                    }
            }
        if (!skipAutostyle) {
            autostyle {
                cpp {
                    licenseHeader(File("${project.rootDir}/LICENSE").readText())
                    trimTrailingWhitespace()
                    endWithNewline()
                    eclipse {
                        configFile("${project.rootDir}/darklaf_cpp.eclipseformat.xml")
                    }
                }
            }
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
            if (!skipJavadoc) {
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
                project.path == ":") {
                // We don't it to Central for now
                return@configure
            }

            publications {
                create<MavenPublication>(project.name) {
                    artifactId = project.name
                    version = rootProject.version.toString()
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
