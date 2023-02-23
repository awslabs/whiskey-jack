/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import com.aws.jag.fxnodeeditor.gengraph.Node;
import com.aws.jag.fxnodeeditor.util.*;
import java.util.*;
import java.util.function.*;
import javafx.geometry.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;
import javafx.scene.text.*;
import javax.annotation.*;

public class NodeView extends Node implements Selectable {
    public NodeView(@Nonnull GraphView parent, @Nonnull Node original) {
        super(parent, original.metadata);
        init();
        populateFrom(original);
    }
    @SuppressWarnings("LeakingThisInConstructor")
    public NodeView(@Nonnull GraphView parent, @Nonnull MetaNode mn) {
        super(parent, mn);
        init();
        try {
            pane.setUserData(this);
            parent.getView().getChildren().add(pane);
            makeDraggable(pane);
        } catch(Throwable t) {
            Dlg.error("Error adding node", t);
        }
    }
    @Override
    public GraphView getContext() {
        return (GraphView) super.getContext();
    }

    private final VBox pane = new VBox();
    private boolean expanded = false;
    private final ImageView openClose = new ImageView(closeArrow);
    private final GridPane contents = new GridPane();
    private final Text title = new Text("unknown");
    private final HBox titleRegion = new HBox(openClose, title);
    private void makeDraggable(final javafx.scene.Node tp) {
        final var dragDelta = new Object() {
            double x;
            double y;
        };
        tp.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            dragDelta.x = tp.getLayoutX() - mouseEvent.getScreenX();
            dragDelta.y = tp.getLayoutY() - mouseEvent.getScreenY();
            tp.setCursor(Cursor.OPEN_HAND);
            mouseEvent.consume();
        });
        tp.setOnMouseReleased(mouseEvent -> {
            tp.setCursor(Cursor.HAND);
            mouseEvent.consume();
        });
        tp.setOnMouseDragged(mouseEvent -> {
            tp.setLayoutX(mouseEvent.getScreenX() + dragDelta.x);
            tp.setLayoutY(mouseEvent.getScreenY() + dragDelta.y);
            getContext().adjustArcs();
            mouseEvent.consume();
        });
        tp.setOnMouseEntered(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.HAND);
            getContext().setHovered(this);
            mouseEvent.consume();
        });
        tp.setOnMouseExited(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.DEFAULT);
            mouseEvent.consume();
            getContext().setHovered(null);
        });
    }
    private void init() {
        openClose.setFitHeight(12);
        openClose.setPreserveRatio(true);
        openClose.getStyleClass().add("arrow");
        HBox.setHgrow(contents, Priority.ALWAYS);
        HBox.setHgrow(titleRegion, Priority.ALWAYS);
        HBox.setHgrow(title, Priority.ALWAYS);
        contents.setHgap(10);
        contents.setPadding(noPadding);
        contents.getColumnConstraints().addAll(flushLeft, flushRight);
        contents.getStyleClass().add("nodeItem");
        pane.setFillWidth(true);
        pane.getStyleClass().setAll("fgpane");
        titleRegion.getStyleClass().setAll("fgtitle");
        title.getStyleClass().setAll("fgtitletext");
        openClose.getStyleClass().setAll("open");
        pane.getChildren().setAll(titleRegion);
        openClose.setOnMouseReleased(e -> setExpanded(!isExpanded()));
        setTitle(metadata.getName());
        setTooltip(metadata.getDescription());
        installPorts();
        setExpanded(true);
    }
    public VBox getView() {
        return pane;
    }
    private void installPorts() {
        ports.values().forEach(new Consumer<Port>() {
            int inrow = 0; // can't do this with a lambda!
            int outrow = 0;
            @Override
            public void accept(Port P) {
                var p = (PortView) P;
                var right = p.metadata.isRightSide();
                contents.add(p.getView(),
                        right ? 0 : 1,
                        right ? inrow++ : outrow++);
            }
        });
    }
    @Override
    public void delete() {
        ports.values().forEach(p->((PortView)p).forEachArc(a->a.delete()));
        getContext().getView().getChildren().remove(getView());
    }
    public void setTitle(String s) {
        title.setText(s);
    }
    private Tooltip tip;
    public void setTooltip(String tooltip) {
        if(!Utils.isEmpty(tooltip))
            if(tip == null) {
                tip = new Tooltip(tooltip);
                Tooltip.install(titleRegion, tip);
            } else
                tip.setText(tooltip);
        else
            if(tip != null) {
                Tooltip.uninstall(titleRegion, tip);
                tip = null;
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
    @Override
    protected void collectMore(Map map) {
        super.collectMore(map);
        putOpt(map, "x", pane.getLayoutX());
        putOpt(map, "y", pane.getLayoutY());
        putOpt(map, "expanded", isExpanded());
    }
    
    private static final Image closeArrow = new Image(NodeView.class.getResourceAsStream("CloseArrow.png"));
    private static final Insets noPadding = new Insets(0, 0, 0, 0);
    private static final ColumnConstraints flushLeft = new ColumnConstraints();
    private static final ColumnConstraints flushRight = new ColumnConstraints();
    static {
        flushLeft.setHalignment(HPos.LEFT);
        flushLeft.setFillWidth(true);
        flushRight.setHalignment(HPos.RIGHT);
        flushRight.setFillWidth(true);
    }
}
