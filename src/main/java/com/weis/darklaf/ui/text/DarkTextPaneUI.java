package com.weis.darklaf.ui.text;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.beans.PropertyChangeEvent;

public class DarkTextPaneUI extends DarkEditorPaneUI {


    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTextPaneUI();
    }

    /*
     * Implementation of BasicEditorPaneUI
     */

    /**
     * Fetches the name used as a key to lookup properties through the
     * UIManager.  This is used as a prefix to all the standard
     * text properties.
     *
     * @return the name ("TextPane")
     */
    protected String getPropertyPrefix() {
        return "TextPane";
    }

    public void installUI(final JComponent c) {
        super.installUI(c);
    }

    /**
     * This method gets called when a bound property is changed
     * on the associated JTextComponent.  This is a hook
     * which UI implementations may change to reflect how the
     * UI displays bound properties of JTextComponent subclasses.
     * If the font, foreground or document has changed, the
     * the appropriate property is set in the default style of
     * the document.
     *
     * @param evt the property change event
     */
    protected void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
    }
}
