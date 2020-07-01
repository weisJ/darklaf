import java.io.FileInputStream
import java.util.Properties

val properties = Properties()
FileInputStream(File("${rootProject.rootDir}/../gradle.properties")).use {
    properties.load(it)
}
properties.stringPropertyNames().forEach {
    extra[it] = properties[it]
}
