package com.github.weisj.darklaf.ui.demo;

import javax.swing.JComponent;
import java.util.Collections;
import java.util.List;

public interface ComponentDemo {

    String getName();

    JComponent getComponent();

    JComponent getContentPane();

    default List<DemoSpec<?>> getSpecs() {
        return Collections.emptyList();
    }

    DemoExecutionSpec getExecutionSpec();
}
