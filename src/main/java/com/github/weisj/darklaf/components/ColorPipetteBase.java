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

package com.github.weisj.darklaf.components;


import com.github.weisj.darklaf.ui.colorchooser.ColorListener;
import com.github.weisj.darklaf.ui.colorchooser.ColorPipette;
import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.plaf.basic.BasicRootPaneUI;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;

public abstract class ColorPipetteBase implements ColorPipette, AWTEventListener {
    protected final JComponent parent;
    protected final Robot robot;
    private final ColorListener colorListener;
    private Runnable closeAction;
    private JWindow pickerWindow;
    private boolean keyDown;
    private int downKeyCode;

    private Color currentColor;
    private Color initialColor;

    public ColorPipetteBase(@NotNull final JComponent parent, @NotNull final ColorListener colorListener) {
        this.parent = parent;
        this.colorListener = colorListener;
        robot = createRobot();
    }

    @Nullable
    private static Robot createRobot() {
        try {
            return new Robot();
        } catch (AWTException e) {
            return null;
        }
    }

    public boolean isKeyDown() {
        return keyDown;
    }

    public int getPressedKeyCode() {
        return downKeyCode;
    }

    public void setCloseAction(final Runnable closeAction) {
        this.closeAction = closeAction;
    }

    protected Color getPixelColor(@NotNull final Point location) {
        return robot.getPixelColor(location.x, location.y);
    }

    @Nullable
    protected Color getInitialColor() {
        return initialColor;
    }

    @Override
    public void setInitialColor(@Nullable final Color initialColor) {
        this.initialColor = initialColor;
        setColor(initialColor);
    }

    @Nullable
    @Override
    public Color getColor() {
        return currentColor;
    }

    protected void setColor(@Nullable final Color color) {
        currentColor = color;
    }

    @Override
    public Window show() {
        Window picker = getOrCreatePickerWindow();
        Toolkit.getDefaultToolkit().addAWTEventListener(this,
                                                        AWTEvent.MOUSE_MOTION_EVENT_MASK
                                                                | AWTEvent.MOUSE_EVENT_MASK
                                                                | AWTEvent.KEY_EVENT_MASK);
        updateLocation();
        picker.setVisible(true);
        return picker;
    }

    @Override
    public void pickAndClose() {
        PointerInfo pointerInfo = MouseInfo.getPointerInfo();
        Color pixelColor = getPixelColor(pointerInfo.getLocation());
        cancelPipette();
        notifyListener(pixelColor);
        setInitialColor(pixelColor);
    }

    @Override
    public void cancelPipette() {
        Window pickerWindow = getPickerWindow();
        if (pickerWindow != null) {
            pickerWindow.setVisible(false);
        }
        Color initialColor = getInitialColor();
        if (initialColor != null) {
            notifyListener(initialColor);
        }
        if (closeAction != null) {
            closeAction.run();
        }
        Toolkit.getDefaultToolkit().removeAWTEventListener(this);
    }

    @NotNull
    protected Window getOrCreatePickerWindow() {
        if (pickerWindow == null) {
            Window owner = SwingUtilities.getWindowAncestor(parent);
            pickerWindow = createPickerWindow(owner);
            pickerWindow.setName("DarkLafPickerDialog");
            JRootPane rootPane = pickerWindow.getRootPane();
            rootPane.putClientProperty("Window.shadow", Boolean.FALSE);
        }
        return pickerWindow;
    }

    @Nullable
    protected Point updateLocation() {
        PointerInfo pointerInfo = MouseInfo.getPointerInfo();
        if (pointerInfo == null) return null;

        Point mouseLocation = pointerInfo.getLocation();
        Window pickerWindow = getPickerWindow();
        if (pickerWindow != null && mouseLocation != null) {
            pickerWindow.setLocation(adjustPickerLocation(mouseLocation, pickerWindow));
        }
        return mouseLocation;
    }

    protected JWindow createPickerWindow(final Window parent) {
        return new PickerWindow(parent);
    }

    @Nullable
    protected Window getPickerWindow() {
        return pickerWindow;
    }

    protected Point adjustPickerLocation(@NotNull final Point mouseLocation, @NotNull final Window pickerWindow) {
        return new Point(mouseLocation.x - pickerWindow.getWidth() / 2,
                         mouseLocation.y - pickerWindow.getHeight() / 2);
    }

    @Override
    public void eventDispatched(@NotNull final AWTEvent event) {
        if (pickerWindow == null || !pickerWindow.isVisible()) return;
        switch (event.getID()) {
            case MouseEvent.MOUSE_PRESSED:
                ((MouseEvent) event).consume();
                pickAndClose();
                break;
            case MouseEvent.MOUSE_CLICKED:
                ((MouseEvent) event).consume();
                break;
            case KeyEvent.KEY_PRESSED:
                downKeyCode = ((KeyEvent) event).getKeyCode();
                switch (downKeyCode) {
                    case KeyEvent.VK_ESCAPE:
                        cancelPipette();
                        break;
                    case KeyEvent.VK_ENTER:
                        pickAndClose();
                        break;
                    default:
                        break;
                }
                if (!keyDown) {
                    keyDown = true;
                    updatePipette(true);
                }
                break;
            case KeyEvent.KEY_RELEASED:
                keyDown = false;
                Window picker = getPickerWindow();
                if (picker != null) {
                    picker.repaint();
                }
                break;
            default:
                break;
        }
    }

    protected abstract void updatePipette(final boolean force);

    protected void notifyListener(@NotNull final Color c) {
        colorListener.colorChanged(c, this);
    }

    @Override
    public boolean imageUpdate(final Image image, final int i, final int i1,
                               final int i2, final int i3, final int i4) {
        return false;
    }

    @Override
    public void dispose() {
        pickerWindow.dispose();
        pickerWindow = null;
        setInitialColor(null);
        setColor(null);
    }

    protected static class PickerWindow extends JWindow {

        protected PickerWindow(final Window parent) {
            super(parent);
            setBackground(DarkUIUtil.TRANSPARENT_COLOR);
            BufferedImage cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
            Cursor blankCursor = Toolkit.getDefaultToolkit()
                                        .createCustomCursor(cursorImg, new Point(), "BlankCursor");
            setCursor(blankCursor);
        }

        @Override
        protected JRootPane createRootPane() {
            return new JRootPane() {
                @Override
                public int getWindowDecorationStyle() {
                    return NONE;
                }

                @Override
                public void updateUI() {
                    setUI(new BasicRootPaneUI());
                }
            };
        }
    }
}
