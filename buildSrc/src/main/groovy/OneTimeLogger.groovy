class OneTimeLogger {
    private final Runnable logger
    private boolean messageAlreadyLogged = false

    OneTimeLogger(Runnable logger) {
        this.logger = logger
    }

    protected void log() {
        if (!messageAlreadyLogged) {
            logger.run()
            messageAlreadyLogged = true
        }
    }
}
