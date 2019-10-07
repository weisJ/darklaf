package com.weis.darklaf.theme;

public class DarculaTheme extends Theme {

    @Override
    public String getName() {
        return "Darcula";
    }

    @Override
    protected String getResourcePath() {
        return "darcula/";
    }

    @Override
    public boolean isDark() {
        return true;
    }
}
