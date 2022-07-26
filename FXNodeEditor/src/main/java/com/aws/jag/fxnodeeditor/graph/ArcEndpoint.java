/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.graph;

import com.aws.jag.fxnodeeditor.meta.*;
import javafx.geometry.*;
import javafx.scene.transform.*;
import javax.annotation.*;

public abstract class ArcEndpoint {
    ArcEndpoint(@Nonnull Port m, @Nonnull FGNode c) {
        meta = m;
        container = c;
        uname = container.uid+":"+meta.name;
    }
    @Nonnull final Port meta;
    @Nonnull final FGNode container;
    final String uname;
    private PortView view;
    public void setView(PortView v) { view = v; }
    public PortView getView() {
        return view;
    }
    @Override public String toString() { return container.meta.name+"."+meta.name; }
    public Point2D getPosition(boolean right, Transform area) {
        try {
            var box = container.view.isExpanded() ? view : container.view;
            var lbl = box.getBoundsInLocal();
            var t = box.getLocalToSceneTransform();
            var ret = t.transform(lbl.getMinX(), lbl.getHeight() / 2);
            if(right) ret = ret.add(lbl.getWidth(), 0);
            return area.inverseTransform(ret);
        } catch(NonInvertibleTransformException ex) {
            Dlg.error("getPosition error", ex);
            return new Point2D(0,0);
        }
    }
    public abstract void setViewText();
}
