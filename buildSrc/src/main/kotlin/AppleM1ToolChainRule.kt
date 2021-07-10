@file:Suppress("UnstableApiUsage")

import org.gradle.api.Action
import org.gradle.internal.os.OperatingSystem
import org.gradle.internal.service.ServiceRegistry
import org.gradle.model.Finalize
import org.gradle.model.RuleSource
import org.gradle.nativeplatform.toolchain.Clang
import org.gradle.nativeplatform.toolchain.GccCommandLineToolConfiguration
import org.gradle.nativeplatform.toolchain.GccPlatformToolChain
import org.gradle.nativeplatform.toolchain.NativeToolChainRegistry
import org.gradle.nativeplatform.toolchain.internal.gcc.DefaultGccPlatformToolChain
import org.gradle.nativeplatform.toolchain.internal.gcc.metadata.GccMetadata
import org.gradle.nativeplatform.toolchain.internal.gcc.metadata.GccMetadataProvider
import org.gradle.nativeplatform.toolchain.internal.metadata.CompilerMetaDataProvider
import org.gradle.process.internal.ExecActionFactory
import java.io.File

class AppleM1ToolChainRule : RuleSource() {
    @Finalize
    fun configureToolchain(toolChains: NativeToolChainRegistry, serviceRegistry: ServiceRegistry) {
        val sdkPath = SdkPathProvider()
        toolChains.create("m1clang", Clang::class.java) {
            val execActionFactory = serviceRegistry.get(ExecActionFactory::class.java)
            target(
                "macosarm64",
                ConfigurePlatformAction(execActionFactory, this, sdkPath)
            )
        }
    }

    private class SdkPathProvider {
        private var sdkPath: Lazy<String?> = lazy {
            "xcrun --sdk macosx --show-sdk-path".runCommand()
        }

        fun get(): String? = sdkPath.value
    }

    private class CompilerMetadataProvider(
        private val clangMetadataProvider: GccMetadataProvider,
        private val toolchain: Clang,
        private val toolConfiguration: GccCommandLineToolConfiguration,
        private val sdkPath: SdkPathProvider
    ) {
        private var metadata: Lazy<GccMetadata> = lazy { loadMetadata() }
        fun get(): GccMetadata = metadata.value

        private fun loadMetadata(): GccMetadata {
            return clangMetadataProvider.getCompilerMetaData(
                toolchain.path
            ) {
                configureCompiler()
            }.component
        }

        private fun CompilerMetaDataProvider.CompilerExecSpec.configureCompiler() {
            args(listOf("-target", "arm64-apple-macos11", "-isysroot", sdkPath.get()))

            // Search in the System path... or in the toolchain path
            if (toolchain.path.isEmpty()) {
                executable(OperatingSystem.current().findInPath(toolConfiguration.executable))
            } else {
                executable(toolchain.path.asSequence()
                    .map { path: File -> createExecutableFile(path) }
                    .filter { obj: File -> obj.isFile }
                    .firstOrNull() ?: throw executableNotFoundException())
            }
        }

        private fun executableNotFoundException(): IllegalStateException {
            return IllegalStateException("Executable '${toolConfiguration.executable}' not found in ${toolchain.path}.")
        }

        private fun createExecutableFile(path: File): File {
            return File(path, toolConfiguration.executable)
        }
    }

    private
    class ConfigureCompilerArgumentsAction(
        private val compilerMetadata: CompilerMetadataProvider,
        private val sdkPath: SdkPathProvider
    ) : Action<MutableList<String>> {
        override fun execute(args: MutableList<String>) {
            val iter = args.listIterator()
            while (iter.hasNext()) {
                val arg = iter.next()
                if (arg == "-isystem") {
                    iter.remove() // remove flag

                    // remove flag value
                    iter.next()
                    iter.remove()
                }
            }
            args.addAll(listOfNotNull("-target", "arm64-apple-macos11", "-isysroot", sdkPath.get()))
            args.addAll(compilerMetadata.get().systemLibraries.includeDirs
                .asSequence()
                .flatMap { listOf("-isystem", it.absolutePath) })
            args.addAll(listOf("-F", sdkPath.get() + "/System/Library/Frameworks"))
        }
    }

    private class ConfigureLinkerArgumentsAction(
        private val linkerMetadata: CompilerMetadataProvider,
        private val sdkPath: SdkPathProvider
    ) : Action<MutableList<String>> {
        override fun execute(args: MutableList<String>) {
            args.addAll(listOfNotNull("-target", "arm64-apple-macos11", "-isysroot", sdkPath.get()))
            args.addAll(linkerMetadata.get().systemLibraries.libDirs
                .asSequence()
                .flatMap { listOf("-L", it.absolutePath) })
            args.addAll(listOf("-F", sdkPath.get() + "/System/Library/Frameworks"))
        }
    }

    private class ConfigurePlatformAction(
        private val execActionFactory: ExecActionFactory,
        private val toolchain: Clang,
        private val sdkPath: SdkPathProvider
    ) : Action<GccPlatformToolChain> {
        private lateinit var metadataProvider: GccMetadataProvider

        override fun execute(platform: GccPlatformToolChain) {
            metadataProvider = GccMetadataProvider.forClang(execActionFactory)
            platform.getcCompiler().withArguments(forCCompiler(platform.getcCompiler()))
            listOf(
                platform.getcCompiler(),
                platform.cppCompiler,
                platform.objcCompiler,
                platform.objcppCompiler
            ).forEach {
                it.configureCompilerArgs()
            }
            platform.linker.withArguments(forLinker(platform.linker))
            configureCompilerProbeArguments(platform)
        }

        private fun GccCommandLineToolConfiguration.configureCompilerArgs() {
            withArguments(forCppCompiler(this))
        }

        private fun forCCompiler(cCompiler: GccCommandLineToolConfiguration): Action<MutableList<String>> {
            val cCompilerMetadata = CompilerMetadataProvider(metadataProvider, toolchain, cCompiler, sdkPath)
            return ConfigureCompilerArgumentsAction(cCompilerMetadata, sdkPath)
        }

        private fun forCppCompiler(cppCompiler: GccCommandLineToolConfiguration): Action<MutableList<String>> {
            val cppCompilerMetadata = CompilerMetadataProvider(metadataProvider, toolchain, cppCompiler, sdkPath)
            return ConfigureCompilerArgumentsAction(cppCompilerMetadata, sdkPath)
        }

        private fun forLinker(linker: GccCommandLineToolConfiguration): Action<MutableList<String>> {
            val linkerMetadata = CompilerMetadataProvider(metadataProvider, toolchain, linker, sdkPath)
            return ConfigureLinkerArgumentsAction(linkerMetadata, sdkPath)
        }

        private fun configureCompilerProbeArguments(platform: GccPlatformToolChain) {
            // Configure compiler probe args, not strictly needed but for completeness.
            (platform as DefaultGccPlatformToolChain).compilerProbeArgs.clear()
            platform.compilerProbeArgs
                .addAll(listOf("-target", "arm64-apple-macos11", "-isysroot", sdkPath.get()))
        }

    }
}
