package com.weis.darklaf.theme;

public class IntelliJTheme extends Theme {

    @Override
    public String getName() {
        return "IntelliJ";
    }

    @Override
    protected String getResourcePath() {
        return "intellij/";
    }

    @Override
    public boolean isDark() {
        return false;
    }
}
