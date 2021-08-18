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
package com.github.weisj.darklaf.transfer;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DragSource;
import java.awt.dnd.DropTarget;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javax.swing.*;

import com.github.weisj.darklaf.properties.icons.IconUtil;
import com.github.weisj.darklaf.util.Pair;

public final class TransferUtil {

    private TransferUtil() {
        throw new IllegalStateException("Utility class");
    }

    public static <T> Pair<DropTarget, AtomicBoolean> setupDnD(final JComponent c, final int sourceActions,
            final Class<T> dataType, final Supplier<T> exporter,
            final Consumer<T> importer, final Function<T, Icon> dragImageCreator) {
        DragSource ds = new DragSource();
        AtomicBoolean dragEnabled = new AtomicBoolean(true);
        TransferHandler handler = new TransferHandler() {
            private final DataFlavor flavor;

            {
                try {
                    flavor = new DataFlavor(
                            DataFlavor.javaJVMLocalObjectMimeType + ";class=" + dataType.getName());
                } catch (final ClassNotFoundException e) {
                    throw new IllegalStateException(e);
                }
            }

            @Override
            public int getSourceActions(final JComponent c) {
                return sourceActions;
            }

            @Override
            public boolean canImport(final TransferSupport support) {
                return support.isDataFlavorSupported(flavor);
            }

            @Override
            public Icon getVisualRepresentation(final Transferable t) {
                try {
                    // noinspection unchecked
                    return dragImageCreator.apply((T) t.getTransferData(flavor));
                } catch (UnsupportedFlavorException | IOException e) {
                    return null;
                }
            }

            @Override
            protected Transferable createTransferable(final JComponent c) {
                T value = exporter.get();
                setDragImage(IconUtil.iconToImage(dragImageCreator.apply(value), c));
                return new ObjectTransferable<>(value, dataType);
            }

            @Override
            public boolean importData(final TransferSupport support) {
                if (!support.isDrop()) return false;
                try {
                    // noinspection unchecked
                    importer.accept((T) support.getTransferable().getTransferData(flavor));
                } catch (final Exception ignored) {
                }
                return false;
            }
        };
        c.setTransferHandler(handler);
        ds.createDefaultDragGestureRecognizer(c, sourceActions,
                dge -> {
                    if (!dragEnabled.get()) return;
                    handler.exportAsDrag(c, dge.getTriggerEvent(), dge.getDragAction());
                });
        return new Pair<>(c.getDropTarget(), dragEnabled);
    }
}
