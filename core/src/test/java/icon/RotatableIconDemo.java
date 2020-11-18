/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package icon;

import java.awt.*;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;
import ui.DemoResources;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.icons.DarkSVGIcon;
import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.Pair;

public class RotatableIconDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new RotatableIconDemo());
    }

    @Override
    public JComponent createComponent() {
        final int size = 25;
        RotatableIcon rotateIcon = new RotatableIcon(((DarkSVGIcon) DemoResources.FOLDER_ICON).derive(size, size));
        rotateIcon.setOrientation(Alignment.EAST);
        JLabel iconLabel = new JLabel(rotateIcon);
        DemoPanel panel = new DemoPanel(iconLabel);

        Alignment[] orientations = new Alignment[8];
        orientations[0] = Alignment.NORTH;
        for (int i = 1; i <= 7; i++) {
            orientations[i] = orientations[i - 1].clockwise();
        }

        int spacing = 360 / orientations.length;
        JSlider slider = new JSlider(0, (orientations.length - 1) * spacing);
        slider.setMajorTickSpacing(spacing);
        slider.setSnapToTicks(true);
        slider.setPaintLabels(true);
        slider.setPaintTicks(true);
        slider.setValue(rotateIcon.getOrientation().getIndex() * spacing);
        slider.addChangeListener(e -> {
            if (!slider.isEnabled()) return;
            rotateIcon.setOrientation(orientations[slider.getValue() / spacing]);
            iconLabel.repaint();
        });

        Timer timer = new Timer(1000, e -> {
            Alignment orientation = rotateIcon.getOrientation().clockwise();
            rotateIcon.setOrientation(orientation);
            slider.setValue(orientation.getDegreeAngle());
            iconLabel.repaint();
        });
        timer.setRepeats(true);

        JCheckBox checkBox = new JCheckBox("Auto rotate");
        checkBox.setSelected(false);
        checkBox.addActionListener(e -> {
            slider.setEnabled(!checkBox.isSelected());
            if (checkBox.isSelected()) {
                rotateIcon.setOrientation(Alignment.NORTH);
                timer.start();
            } else {
                timer.stop();
            }
        });

        JComponent controls = panel.addControls(new GridBagLayout());
        controls.add(slider);
        controls = panel.addControls(Box.createHorizontalBox());
        controls.add(checkBox);
        controls.add(Box.createHorizontalGlue());
        controls = panel.addControls(new BorderLayout());

        AtomicReference<Icon> nextIcon = new AtomicReference<>(rotateIcon.getIcon());
        JList<Pair<String, ? extends Icon>> list = AllIcons.createIconJList(size);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.addListSelectionListener(e -> nextIcon.set(list.getSelectedValue().getSecond()));
        controls.add(new OverlayScrollPane(list));
        Box box = Box.createHorizontalBox();
        box.add(Box.createHorizontalGlue());
        JButton apply = new JButton("Change Icon");
        apply.addActionListener(e -> {
            rotateIcon.setIcon(nextIcon.get());
            iconLabel.repaint();
        });
        box.add(apply);
        box.add(Box.createHorizontalGlue());
        controls = panel.addControls(new BorderLayout());
        controls.add(box);
        return panel;
    }

    @Override
    public String getTitle() {
        return "RotatableIcon Demo";
    }
}
