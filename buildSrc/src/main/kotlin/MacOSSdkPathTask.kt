import org.gradle.api.DefaultTask
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import org.gradle.nativeplatform.toolchain.internal.xcode.MacOSSdkPathLocator
import java.io.File
import javax.inject.Inject

open class MacOSSdkPathTask @Inject constructor(
    private val locator: MacOSSdkPathLocator
) : DefaultTask() {
    @Internal
    lateinit var sdkPath: File

    @TaskAction
    fun run() {
        sdkPath = locator.find()
    }
}
