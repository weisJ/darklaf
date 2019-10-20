package com.weis.darklaf.components.tristate;

import javax.swing.*;
import java.awt.event.ItemEvent;

public class TristateButtonModel extends JToggleButton.ToggleButtonModel {
    private TristateState state = TristateState.DESELECTED;

    public TristateButtonModel() {
        this(TristateState.DESELECTED);
    }

    public TristateButtonModel(final TristateState state) {
        setState(state);
    }

    protected void displayState() {
        super.setSelected(state != TristateState.DESELECTED);
        super.setArmed(state == TristateState.INDETERMINATE);
        super.setPressed(state == TristateState.INDETERMINATE);
    }

    public void setIndeterminate() {
        setState(TristateState.INDETERMINATE);
    }

    public boolean isIndeterminate() {
        return state == TristateState.INDETERMINATE;
    }

    @Override
    public void setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
        // Restore state display
        displayState();
    }

    @Override
    public boolean isSelected() {
        return state == TristateState.SELECTED;
    }

    protected void iterateState() {
        setState(state.next());
    }

    public TristateState getState() {
        return state;
    }

    public void setState(final TristateState state) {
        this.state = state;
        displayState();
        if (state == TristateState.INDETERMINATE && isEnabled()) {
            fireStateChanged();
            int indeterminate = 3;
            //noinspection MagicConstant
            fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED, this, indeterminate));
        }
    }
}
