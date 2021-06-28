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
package com.github.weisj.darklaf.ui;

import java.awt.*;
import java.util.Arrays;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.graphics.DefaultInterpolator;
import com.github.weisj.darklaf.graphics.Interpolator;

public class AnimationDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new AnimationDemo());
    }

    @Override
    public JComponent createComponent() {
        List<Interpolator> interpolators = Arrays.asList(DefaultInterpolator.values());
        JList<Interpolator> listComp = new JList<>();
        DefaultListModel<Interpolator> listModel = new DefaultListModel<>();
        interpolators.forEach(listModel::addElement);
        listComp.setModel(listModel);
        listComp.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        AnimatedElement animatedElement = new AnimatedElement();
        listComp.setSelectedValue(animatedElement.animator.getInterpolator(), true);

        JPanel top = new JPanel(new GridBagLayout());
        JPanel topHolder = new JPanel(new BorderLayout());
        topHolder.add(top, BorderLayout.CENTER);
        top.add(animatedElement, null);

        Box buttonHolder = Box.createHorizontalBox();
        buttonHolder.add(Box.createHorizontalGlue());

        JButton runButton = new JButton("Run");

        runButton.addActionListener(e -> {
            runButton.setEnabled(false);
            listComp.setEnabled(false);
            animatedElement.animator.setInterpolator(listComp.getSelectedValue());
            animatedElement.animator.suspend();
            animatedElement.animator.resume();
        });
        animatedElement.endCallback = () -> {
            runButton.setEnabled(true);
            listComp.setEnabled(true);
        };

        buttonHolder.add(runButton);
        buttonHolder.add(Box.createHorizontalGlue());

        topHolder.add(buttonHolder, BorderLayout.SOUTH);

        JPanel content = new JPanel(new BorderLayout());
        content.add(topHolder, BorderLayout.CENTER);
        content.add(listComp, BorderLayout.SOUTH);
        return content;
    }

    @Override
    public String getTitle() {
        return "Animation Demo";
    }

    private static class AnimatedElement extends JPanel {
        private float state;

        private Runnable endCallback;

        Animator animator = new Animator(1000, 1000, 0) {
            @Override
            public void paintNow(final float fraction) {
                state = fraction;
                paintImmediately(getVisibleRect());
            }

            @Override
            protected void paintCycleEnd() {
                super.paintCycleEnd();
                if (endCallback != null) endCallback.run();
            }

            @Override
            public void suspend() {
                super.suspend();
                paintImmediately(getVisibleRect());
            }
        };

        @Override
        protected void paintComponent(final Graphics g) {
            super.paintComponent(g);
            g.setColor(getForeground());
            int x = (int) ((getWidth() - 5) * state);
            g.fillRect(x, 0, 5, getHeight());
        }

        @Override
        public Dimension getPreferredSize() {
            return new Dimension(200, 50);
        }

    }
}
