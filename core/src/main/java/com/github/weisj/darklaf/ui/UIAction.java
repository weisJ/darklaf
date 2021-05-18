package com.github.weisj.darklaf.ui;

import javax.swing.Action;
import java.beans.PropertyChangeListener;

public abstract class UIAction implements Action {
    private final String name;

    public UIAction(String name) {
        this.name = name;
    }

    public final String getName() {
        return name;
    }

    public Object getValue(String key) {
        if (NAME.equals(key)) {
            return name;
        }
        return null;
    }

    // UIAction is not mutable, this does nothing.
    public void putValue(String key, Object value) {}

    // UIAction is not mutable, this does nothing.
    public void setEnabled(boolean b) {}

    /**
     * Cover method for <code>isEnabled(null)</code>.
     */
    public final boolean isEnabled() {
        return accept(null);
    }

    /**
     * Subclasses that need to conditionalize the enabled state should override this. Be aware that
     * <code>sender</code> may be null.
     *
     * @param sender Widget enabled state is being asked for, may be null.
     */
    @Override
    public boolean accept(Object sender) {
        return true;
    }

    // UIAction is not mutable, this does nothing.
    public void addPropertyChangeListener(PropertyChangeListener listener) {}

    // UIAction is not mutable, this does nothing.
    public void removePropertyChangeListener(PropertyChangeListener listener) {}
}
