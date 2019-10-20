package com.weis.darklaf.theme;

/**
 * @author Jannis Weis
 */
public class DarculaTheme extends Theme {

    @Override
    protected String getResourcePath() {
        return "darcula/";
    }

    @Override
    public String getName() {
        return "Darcula";
    }

    @Override
    public boolean isDark() {
        return true;
    }
}
