class OneTimeLogger {
    private final Runnable logger
    private boolean messageAlreadyLogged = false

    OneTimeLogger(Runnable logger) {
        this.logger = logger
    }

    protected boolean isLogged() {
        return messageAlreadyLogged
    }

    protected void setLogged(boolean logged) {
        messageAlreadyLogged = logged
    }

    protected void log() {
        if (!isLogged()) {
            logger.run()
            setLogged(true)
        }
    }

    static class Static extends OneTimeLogger {

        private static final Map<Object, Boolean> isLogged = new HashMap<>()

        private final Object identifier

        Static(Runnable logger, Object identifier) {
            super(logger)
            this.identifier = identifier
        }

        @Override
        protected void setLogged(boolean logged) {
            isLogged.put(identifier, logged)
        }

        @Override
        protected boolean isLogged() {
            return Boolean.TRUE == isLogged.get(identifier)
        }
    }
}
