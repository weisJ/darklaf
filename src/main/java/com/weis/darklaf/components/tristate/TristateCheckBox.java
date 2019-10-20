package com.weis.darklaf.components.tristate;

import com.weis.darklaf.DarkLaf;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class TristateCheckBox extends JCheckBox {
    private final ChangeListener enableListener = e -> TristateCheckBox.this.setFocusable(getModel().isEnabled());

    public TristateCheckBox(final String text) {
        this(text, null, TristateState.DESELECTED);
    }

    public TristateCheckBox(final String text, final Icon icon, final TristateState initial) {
        super(text, icon);
        setModel(new TristateButtonModel(initial));
        // override action behaviour
        super.addMouseListener(new MouseAdapter() {
            public void mousePressed(final MouseEvent e) {
                TristateCheckBox.this.iterateState();
            }
        });
    }

    private void iterateState() {
        if (!getModel().isEnabled()) return;

        grabFocus();
        getTristateModel().iterateState();
        repaint();

        int modifiers = 0;
        AWTEvent currentEvent = EventQueue.getCurrentEvent();
        if (currentEvent instanceof InputEvent) {
            modifiers = ((InputEvent) currentEvent).getModifiersEx();
        } else if (currentEvent instanceof ActionEvent) {
            modifiers = ((ActionEvent) currentEvent).getModifiers();
        }
        fireActionPerformed(new ActionEvent(this,
                                            ActionEvent.ACTION_PERFORMED, getText(),
                                            System.currentTimeMillis(), modifiers));
    }

    public TristateButtonModel getTristateModel() {
        return (TristateButtonModel) super.getModel();
    }

    @NotNull
    @Contract(pure = true)
    public String getUIClassID() {
        if (UIManager.getLookAndFeel() instanceof DarkLaf) {
            return "TristateCheckBoxUI";
        } else {
            return super.getUIClassID();
        }
    }

    @Override
    public void setSelected(final boolean b) {
        setState(b ? TristateState.SELECTED : TristateState.DESELECTED);
    }

    @Override
    public void setModel(final ButtonModel newModel) {
        super.setModel(newModel);

        if (model instanceof TristateButtonModel) {
            model.addChangeListener(enableListener);
        }
    }

    public void setIndeterminate() {
        getTristateModel().setIndeterminate();
    }

    public boolean isIndeterminate() {
        return getTristateModel().isIndeterminate();
    }

    public TristateState getState() {
        return getTristateModel().getState();
    }

    public void setState(final TristateState state) {
        getTristateModel().setState(state);
    }
}
