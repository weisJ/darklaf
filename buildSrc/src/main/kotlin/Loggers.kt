import java.io.File
import java.util.concurrent.Callable

class CallableAction(action: () -> Unit) : OneTimeAction(action), Callable<List<File>> {

    override fun call(): List<File> {
        this.execute()
        return emptyList()
    }
}


open class OneTimeAction(private val action: () -> Unit) {
    internal open var alreadyExecuted = false

    fun execute() {
        if (alreadyExecuted) return
        alreadyExecuted = true
        action()
    }

    companion object {

        private val isExecutedMap = mutableMapOf<String, Boolean>()

        fun createGlobal(name: String, action: () -> Unit): OneTimeAction {
            isExecutedMap.putIfAbsent(name, false)
            return object : OneTimeAction(action) {
                override var alreadyExecuted
                    get() = isExecutedMap[name] ?: false
                    set(value) {
                        isExecutedMap[name] = value
                    }
            }
        }
    }
}
