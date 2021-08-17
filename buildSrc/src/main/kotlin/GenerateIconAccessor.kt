import java.io.File
import java.io.FileInputStream
import java.util.*

fun createIconAccessor(propertyFile: File, packageName : String, className: String): String {
    class Property(val name: String?, val path: String)
    class AccessorTreeNode(
        val nodes: MutableMap<String, AccessorTreeNode>,
        val properties: MutableList<Property>
    )

    val props = Properties().apply {
        this.load(FileInputStream(propertyFile))
    }
    val root = AccessorTreeNode(mutableMapOf(), mutableListOf())
    props.forEach { (k, value) ->
        var key = k.toString()
        val sepIndex = key.indexOf('|')
        val name = if (sepIndex >= 0) {
            val end = key.substring(sepIndex + 1)
            key = key.substring(0, sepIndex)
            end
        } else null
        val segments = key.split(".")

        var node = root
        for (segment in segments) {
            node = node.nodes.getOrPut(segment) {
                AccessorTreeNode(mutableMapOf(), mutableListOf())
            }
        }
        node.properties.add(Property(name, value.toString()))
    }

    fun createAccessor(prop: Property): String =
        """
        public static Icon ${prop.name ?: "get"}() {
            return IconSet.iconLoader().getIcon("${prop.path}", true);
        }

        public static Icon ${prop.name ?: "get"}(final int width, final int height) {
            return IconSet.iconLoader().getIcon("${prop.path}", width, height, true);
        }
        """.trimIndent()

    fun createAccessorClass(name: String, node: AccessorTreeNode, topLevel: Boolean = false): String {
        val properties = node.properties.sortedBy { it.name }.joinToString(separator = "\n\n") {
            createAccessor(it)
        }.replace("\n", "\n    ")
        val subNodes = node.nodes.entries.asSequence().sortedBy { it.key }.joinToString(separator = "\n\n") {
            createAccessorClass(it.key, it.value)
        }.replace("\n", "\n    ")
        return """
            |@javax.annotation.Generated(value = {"GenerateIconAccessor"})
            |public ${if (topLevel) "" else "static "}final class ${name.capitalize()} {
            |    $properties
            |    $subNodes
            |}
            """.trimMargin()
    }

    return """
        |package $packageName;
        |
        |import javax.swing.Icon;
        |
        |${createAccessorClass(className, root, topLevel = true)}
        """.trimMargin()
}
