package com.weis.darklaf.ui.text;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Element;
import javax.swing.text.View;
import java.beans.PropertyChangeEvent;

/**
 * @author Jannis Weis
 */
public class DarkTextPaneUI extends DarkEditorPaneUI {


    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTextPaneUI();
    }

    /*
     * Implementation of BasicEditorPaneUI
     */

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
    }

    @Override
    protected void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
    }

    @Override
    protected String getPropertyPrefix() {
        return "TextPane";
    }

    @Override
    public View create(final Element elem) {
        return super.create(elem);
    }
}
