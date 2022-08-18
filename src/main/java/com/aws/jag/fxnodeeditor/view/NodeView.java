/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import com.aws.jag.fxnodeeditor.util.*;
import java.util.function.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;
import javafx.scene.text.*;
import javax.annotation.*;

class NodeView extends Node {
    public NodeView(@Nonnull Graph parent, @Nonnull NodeView original) {
        super(parent, original);
        init();
    }
    public NodeView(@Nonnull Graph parent, @Nonnull MetaNode mn) {
        super(parent, mn);
        init();
    }
    @Override
    public View getContext() {
        return (View)super.getContext();
    }

    private final VBox pane = new VBox();
    private boolean expanded = false;
    private final ImageView openClose = new ImageView(closeArrow);
    private final GridPane contents = new GridPane();
    private final Text title = new Text("unknown");
    private final HBox titleRegion = new HBox(openClose, title);
    private void init() {
        openClose.setFitHeight(12);
        openClose.setPreserveRatio(true);
        openClose.getStyleClass().add("arrow");
        HBox.setHgrow(contents, Priority.ALWAYS);
        HBox.setHgrow(titleRegion, Priority.ALWAYS);
        HBox.setHgrow(title, Priority.ALWAYS);
        pane.setFillWidth(true);
        pane.getStyleClass().setAll("fgpane");
        titleRegion.getStyleClass().setAll("fgtitle");
        title.getStyleClass().setAll("fgtitletext");
        openClose.getStyleClass().setAll("open");
        pane.getChildren().setAll(titleRegion);
        openClose.setOnMouseReleased(e -> setExpanded(!isExpanded()));
        setTitle(metadata.name);
        setTooltip(metadata.description);
        installPorts();
        setExpanded(true);
    }
    private void installPorts() {
        ports.forEach(new Consumer<Port>() {
            int inrow = 0; // can't do this with a lambda!
            int outrow = 0;
            @Override
            public void accept(Port P) {
                var p = (PortView) P;
                var in = p.metadata.in;
                contents.add(p.getView(),
                        in ? 0 : 1,
                        in ? inrow++ : outrow++);
            }
        });
    }
    public void setTitle(String s) {
        title.setText(s);
    }
    private Tooltip tip;
    public void setTooltip(String tooltip) {
        if(!Utils.isEmpty(tooltip)) {
            if(tip == null) {
                tip = new Tooltip(tooltip);
                Tooltip.install(titleRegion, tip);
            }
            else tip.setText(tooltip);
        }
        else {
            if(tip!=null) {
                Tooltip.uninstall(titleRegion, tip);
                tip = null;
            }
        }
    }
    public final void setExpanded(boolean b) {
        if(b != expanded) {
            expanded = b;
            openClose.getStyleClass().setAll(expanded ? "fgopen" : "fgclosed");
            pane.getChildren().remove(contents);
            if(expanded)
                pane.getChildren().add(contents);
            openClose.setRotate(expanded ? 90 : 0);
            pane.resize(pane.getPrefWidth(), expanded ? pane.getPrefHeight() : 30);
            getContext().adjustArcs();
        }
    }
    public boolean isExpanded() {
        return expanded;
    }
    static final private Image closeArrow = new Image(NodeView.class.getResourceAsStream("CloseArrow.png"));

}
