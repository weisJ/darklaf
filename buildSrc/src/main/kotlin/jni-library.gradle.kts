
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

val TargetMachine.getVariantName: String get() = "$operatingSystemFamily-$architecture"

// Gradle populates library.binaries in afterEvaluate, so we can't access it earlier
afterEvaluate {
    // C++ library is built for Windows only, so we skip it otherwise
    library.developmentBinary.orNull?.let { it as CppSharedLibrary }?.let { developmentBinary ->
        tasks.test {
            dependsOn(developmentBinary.linkTask)
            val libraryDir = developmentBinary.runtimeFile
                .map { it.asFile.parentFile.absolutePath }
            systemProperty("java.library.path", libraryDir.overrideToString())
        }
    }
    tasks.jar {
        // Publish non-optimized, debuggable binary to simplify analysis in case of crashes
        val libraryPath = "com/github/weisj/darklaf/platform/${project.name}"
        library.binaries.get()
            .filter { it.isOptimized }
            .filterIsInstance<CppSharedLibrary>().let {
                if (it.isEmpty()) {
                    library.targetMachines.get().forEach { targetMachine ->
                        val variantName = targetMachine.variantName
                        val libraryFile = file("libraries/$variantName/$defaultLibraryName")
                        if (!libraryFile.exists()) {
                            logger.warn(
                                "Library $libraryFile for targetMachine $variantName does not exist. Download it from "
                                        + "https://github.com/weisJ/darklaf"
                            )
                        } else {
                            into("$libraryPath/$variantName") {
                                from(libraryFile)
                            }
                        }
                    }
                } else {
                    it.forEach { binary ->
                        binary.linkTask.get().debuggable.set(false)
                        dependsOn(binary.linkTask)
                        val variantName = getVariantName(binary.targetMachine)
                        into("$libraryPath/$variantName") {
                            from(binary.runtimeFile)
                        }
                    }
                }
            }
    }
}
