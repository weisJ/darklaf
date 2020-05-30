import java.util.concurrent.Callable

class CallableLogger extends OneTimeLogger implements Callable<List<File>> {

    CallableLogger(Runnable logger) {
        super(logger)
    }

    @Override
    List<File> call() throws Exception {
        super.log()
        return Collections.emptyList()
    }
}
