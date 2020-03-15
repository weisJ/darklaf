import org.gradle.internal.jvm.Jvm
import org.gradle.kotlin.dsl.invoke

plugins {
    `cpp-library`
    `java-library`
}

// This configuration might be used for adding cpp-only dependencies
val jniImplementation by configurations.creating

val defaultLibraryName: String by project

configurations.matching {
    it.name.startsWith("cppCompile") ||
            it.name.startsWith("nativeLink") ||
            it.name.startsWith("nativeRuntime")
}.all {
    extendsFrom(jniImplementation)
}

tasks.compileJava {
    options.headerOutputDirectory.convention(
        project.layout.buildDirectory.dir("generated/jni-headers")
    )
    // The nested output is not marked automatically as an output of the task regarding task dependencies.
    // So we mark it manually here.
    // See https://github.com/gradle/gradle/issues/6619.
    outputs.dir(options.headerOutputDirectory)
    // Cannot do incremental header generation, since the pattern for cleaning them up is currently wrong.
    // See https://github.com/gradle/gradle/issues/12084.
    options.isIncremental = false
}

tasks.withType<CppCompile>().configureEach {
    includes(tasks.compileJava.flatMap { it.options.headerOutputDirectory })
}

library {
    binaries.configureEach {
        val targetOs = targetMachine.operatingSystemFamily
        compileTask.get().apply {
            val javaHome = Jvm.current().javaHome.canonicalPath
            includes("$javaHome/include")
            includes(when {
                targetOs.isMacOs -> listOf("$javaHome/include/darwin")
                targetOs.isLinux -> listOf("$javaHome/include/linux")
                targetOs.isWindows -> listOf("$javaHome/include/win32")
                else -> emptyList()
            })
        }
    }
}

/**
 * Gradle does not support [Provider] for [JavaForkOptions.systemProperty], so
 * we pass an object that overrides [Any.toString].
 */
fun Provider<String>.overrideToString() = object {
    override fun toString() = orNull ?: ""
}

val TargetMachine.variantName: String get() = "$operatingSystemFamily-$architecture"

// Gradle populates library.binaries in afterEvaluate, so we can't access it earlier
afterEvaluate {
    // C++ library is built for Windows/macOS only, so we skip it otherwise
    library.developmentBinary.orNull?.let { it as CppSharedLibrary }?.let { developmentBinary ->
        tasks.test {
            dependsOn(developmentBinary.linkTask)
            val libraryDir = developmentBinary.runtimeFile
                .map { it.asFile.parentFile.absolutePath }
            systemProperty("java.library.path", libraryDir.overrideToString())
        }
    }
    tasks.jar {
        //Disable all task. Tasks are reenabled if needed.
        library.binaries.get().forEach {
            it.compileTask.get().enabled = false
        }
        library.binaries.get()
            .filter { it.isOptimized }
            .filterIsInstance<CppSharedLibrary>().let {
                val taskMap = it.map { binary -> binary.targetPlatform.targetMachine to binary }.toMap()
                library.targetMachines.get().forEach { targetMachine ->
                    val libraryPath = "com/github/weisj/darklaf/platform/${project.name}"
                    val variantName = targetMachine.variantName
                    val libraryFile = file("libraries/$variantName/$defaultLibraryName")
                    val relativePath = rootProject.relativePath(libraryFile)
                    when {
                        libraryFile.exists() -> {
                            //Use provided library.
                            logger.warn(
                                "${project.name}: Using pre-build library $relativePath for targetMachine $variantName."
                            )
                            into("$libraryPath/$variantName") {
                                from(libraryFile)
                            }
                        }
                        targetMachine in taskMap -> {
                            taskMap[targetMachine]?.let { binary ->
                                binary.compileTask.get().enabled = true
                                // Publish optimized binary to reduce size.
                                binary.linkTask.get().debuggable.set(false)
                                //Build and copy library
                                dependsOn(binary.linkTask)
                                into("$libraryPath/$variantName") {
                                    from(binary.runtimeFile)
                                }
                            }
                        }
                        else -> {
                            val downloadUrl =
                                "https://github.com/weisJ/darklaf/actions?query=workflow%3A%22Build+Native+Libraries%22+branch%3Amaster"
                            logger.warn(
                                """
                                ${project.name}: Library $relativePath for targetMachine $variantName does not exist.
                                ${" ".repeat(project.name.length + 1)} Download it from $downloadUrl
                                """.trimIndent()
                            )
                        }
                    }
                }
            }
    }
}
