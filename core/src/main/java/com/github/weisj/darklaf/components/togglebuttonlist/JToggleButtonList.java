/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.components.togglebuttonlist;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.util.Actions;

public class JToggleButtonList extends JList<JToggleButton> {

    public JToggleButtonList() {
        super.setModel(new DefaultListModel<>());
        super.setCellRenderer(new ToggleButtonListCellRenderer());

        addMouseListener(new MouseAdapter() {

            @Override
            public void mousePressed(final MouseEvent e) {
                int index = locationToIndex(e.getPoint());
                Rectangle bounds = getCellBounds(index, index);
                if (index != -1) {
                    JToggleButton toggleButton = getModel().getElementAt(index);
                    // check if the click is on the togglebutton.
                    Point p = e.getPoint();
                    boolean inside = toggleButton.contains(p.x - bounds.x, p.y - bounds.y);

                    if (e.getClickCount() >= 2 || inside) {
                        toggleButton.setSelected(!toggleButton.isSelected());
                        fireSelectionValueChanged(index, index, inside);
                    }
                    repaint();
                }
            }
        });

        InputMap map = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        map.put(KeyStroke.getKeyStroke("released SPACE"), "toggle_togglebutton");
        getActionMap().put("toggle_togglebutton", Actions.create("toggle_togglebutton", e -> {
            int leadIndex = getSelectionModel().getLeadSelectionIndex();
            if (leadIndex >= 0) setSelected(leadIndex, !JToggleButtonList.this.isSelected(leadIndex));
        }));
        super.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    }

    public void addToggleButton(final String text) {
        addToggleButton(text, false);
    }

    public void addToggleButton(final String text, final boolean selected) {
        addToggleButton(getModel().getSize(), text, selected);
    }

    public void addToggleButton(final int index, final String text) {
        addToggleButton(index, text, false);
    }

    public void addToggleButton(final int index, final String text, final boolean selected) {
        addToggleButton(index, new CheckBoxListItem(text, selected));
    }

    public void addToggleButton(final JToggleButton item) {
        addToggleButton(getModel().getSize(), item);
    }

    public void addToggleButton(final int index, final JToggleButton item) {
        getToggleButtonModel().add(index, item);
    }

    public boolean isSelected(final int index) {
        return getToggleButtonModel().get(index).isSelected();
    }

    public void setSelected(final int index, final boolean selected) {
        getToggleButtonModel().get(index).setSelected(selected);
        repaint(getCellBounds(index, index));
    }

    private DefaultListModel<JToggleButton> getToggleButtonModel() {
        return (DefaultListModel<JToggleButton>) getModel();
    }

    @Override
    public void updateUI() {
        super.updateUI();
        if (getModel() instanceof DefaultListModel) {
            DefaultListModel<JToggleButton> model = getToggleButtonModel();
            for (int i = 0; i < model.getSize(); i++) {
                model.get(i).updateUI();
            }
        }
    }

    /**
     * Gets all the indices that are checked.
     *
     * @return Checked Indices
     */
    public int[] getCheckedIndices() {
        List<Integer> list = new ArrayList<>();
        ListModel<JToggleButton> dlm = getModel();
        for (int i = 0; i < dlm.getSize(); ++i) {
            JToggleButton togglebutton = getModel().getElementAt(i);
            if (togglebutton.isSelected()) {
                list.add(i);
            }
        }
        return list.stream().mapToInt(Integer::intValue).toArray();
    }

    /**
     * Gets Checked Items
     *
     * @return Checked Items
     */
    public List<JToggleButton> getCheckedItems() {
        ArrayList<JToggleButton> list = new ArrayList<>();
        ListModel<JToggleButton> dlm = getModel();
        for (int i = 0; i < dlm.getSize(); ++i) {
            JToggleButton togglebutton = dlm.getElementAt(i);
            if (togglebutton.isSelected()) {
                list.add(togglebutton);
            }
        }
        return list;
    }

    @Override
    public void setModel(final ListModel<JToggleButton> model) {}

    @Override
    public void setSelectionMode(final int selectionMode) {}

    @Override
    public void setSelectionModel(final ListSelectionModel selectionModel) {}

    @Override
    public void setCellRenderer(final ListCellRenderer<? super JToggleButton> cellRenderer) {}
}
