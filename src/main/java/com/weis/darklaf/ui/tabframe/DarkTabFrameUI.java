/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.weis.darklaf.ui.tabframe;

import com.weis.darklaf.components.border.MutableLineBorder;
import com.weis.darklaf.components.tabframe.PopupContainer;
import com.weis.darklaf.components.tabframe.TabFrame;
import com.weis.darklaf.components.tabframe.TabFramePopup;
import com.weis.darklaf.components.tabframe.TabFrameUI;
import com.weis.darklaf.util.DarkUIUtil;
import org.jdesktop.jxlayer.JXLayer;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.pbjar.jxlayer.plaf.ext.transform.DefaultTransformModel;
import org.pbjar.jxlayer.plaf.ext.transform.TransformUtils;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.MouseEvent;

/**
 * UI class for {@link TabFrame}.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class DarkTabFrameUI extends TabFrameUI implements AWTEventListener {

    private TabFrame tabFrame;

    private JXLayer<JComponent> rotatePaneLeft;
    private JXLayer<JComponent> rotatePaneRight;

    private LayoutManager layout;
    private MutableLineBorder topBorder;
    private MutableLineBorder bottomBorder;
    private MutableLineBorder leftBorder;
    private MutableLineBorder rightBorder;
    private Color lineColor;
    private int tabHeight;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabFrameUI();
    }

    @Override
    public void installUI(@NotNull final JComponent c) {
        tabFrame = (TabFrame) c;
        installDefaults();
        installComponents();
        installListeners();
    }

    protected void installDefaults() {
        layout = createLayout();
        tabFrame.setLayout(layout);
        lineColor = UIManager.getColor("TabFrame.line");
        tabHeight = UIManager.getInt("TabFrame.tabHeight");
        topBorder = new MutableLineBorder.UIResource(0, 0, 1, 0, lineColor);
        bottomBorder = new MutableLineBorder.UIResource(1, 0, 0, 0, lineColor);
        rightBorder = new MutableLineBorder.UIResource(0, 0, 1, 0, lineColor);
        leftBorder = new MutableLineBorder.UIResource(0, 0, 1, 0, lineColor);

        tabFrame.getTopTabContainer().setBorder(topBorder);
        tabFrame.getBottomTabContainer().setBorder(bottomBorder);
        tabFrame.getRightTabContainer().setBorder(rightBorder);
        tabFrame.getLeftTabContainer().setBorder(leftBorder);
    }

    private void installComponents() {
        DefaultTransformModel rightTransformModel = new DefaultTransformModel();
        rightTransformModel.setQuadrantRotation(1);
        rightTransformModel.setScaleToPreferredSize(true);
        rotatePaneRight = TransformUtils.createTransformJXLayer(tabFrame.getRightTabContainer(), rightTransformModel);

        DefaultTransformModel leftTransformModel = new DefaultTransformModel();
        leftTransformModel.setQuadrantRotation(3);
        leftTransformModel.setScaleToPreferredSize(true);
        rotatePaneLeft = TransformUtils.createTransformJXLayer(tabFrame.getLeftTabContainer(), leftTransformModel);

        tabFrame.add(tabFrame.getTopTabContainer());
        tabFrame.add(tabFrame.getBottomTabContainer());
        tabFrame.add(rotatePaneLeft);
        tabFrame.add(rotatePaneRight);
    }

    protected void installListeners() {
        Toolkit.getDefaultToolkit().addAWTEventListener(this, AWTEvent.MOUSE_EVENT_MASK);
    }

    protected LayoutManager createLayout() {
        return new TabFrameLayout(tabFrame, this);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        tabFrame.remove(tabFrame.getTopTabContainer());
        tabFrame.remove(tabFrame.getBottomTabContainer());
        tabFrame.remove(rotatePaneLeft);
        tabFrame.remove(rotatePaneRight);
        layout = null;
        lineColor = null;
        topBorder = null;
        bottomBorder = null;
        leftBorder = null;
        rightBorder = null;
        rotatePaneLeft = null;
        rotatePaneRight = null;
        uninstallListeners();
    }

    protected void uninstallListeners() {
        Toolkit.getDefaultToolkit().removeAWTEventListener(this);
    }

    public void updateBorders(final int topCount, final int bottomCount,
                              final int leftCount, final int rightCount) {
        leftBorder.setRight(topCount > 0 ? 0 : 1);
        leftBorder.setLeft(bottomCount > 0 ? 0 : 1);
        rightBorder.setLeft(topCount > 0 ? 0 : 1);
        rightBorder.setRight(bottomCount > 0 ? 0 : 1);
    }

    public int getTabSize(final TabFrame tabFrame) {
        return tabHeight;
    }

    public JComponent getLeftContainer() {
        return rotatePaneLeft;
    }

    public JComponent getRightContainer() {
        return rotatePaneRight;
    }

    public JComponent getTopContainer() {
        return tabFrame.getTopTabContainer();
    }

    public JComponent getBottomContainer() {
        return tabFrame.getBottomTabContainer();
    }

    @Override
    public void eventDispatched(@NotNull final AWTEvent event) {
        if (event.getID() == MouseEvent.MOUSE_PRESSED) {
            var e = (MouseEvent) event;
            var comp = e.getComponent().getComponentAt(e.getPoint());
            var parent = DarkUIUtil.getParentOfType(TabFramePopup.class, comp);
            if (parent != null) {
                parent.getComponent().requestFocus();
                return;
            }
            var parent2 = DarkUIUtil.getParentOfType(PopupContainer.class, comp);
            if (parent2 != null) {
                parent2.getPopup().requestFocus();
                return;
            }
            var parent3 = DarkUIUtil.getParentOfType(TabFrame.class, comp);
            if (parent3 != null && comp != null && !comp.hasFocus()) {
                parent3.requestFocus();
            }
        }
    }
}
