/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.util.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;
import javafx.scene.text.*;

public class NodePane extends VBox {
    private boolean expanded = false;
    private final ImageView openClose;
    private final GridPane contents;
    private final FGNode outer;
    private final HBox titleRegion;
    NodePane(String title, String tooltip, GridPane c, final FGNode outer) {
        this.outer = outer;
        contents = c;
        javafx.scene.text.Text text = new Text(title);
        openClose = new ImageView(closeArrow);
        openClose.setFitHeight(12);
        openClose.setPreserveRatio(true);
        openClose.getStyleClass().add("arrow");
        titleRegion = new HBox(openClose, text);
        if(!Utils.isEmpty(tooltip))
            Tooltip.install(titleRegion, new Tooltip(tooltip));
        HBox.setHgrow(c, Priority.ALWAYS);
        HBox.setHgrow(titleRegion, Priority.ALWAYS);
        HBox.setHgrow(text, Priority.ALWAYS);
        setFillWidth(true);
        getStyleClass().setAll("fgpane");
        titleRegion.getStyleClass().setAll("fgtitle");
        text.getStyleClass().setAll("fgtitletext");
        openClose.getStyleClass().setAll("open");
        getChildren().setAll(titleRegion);
        openClose.setOnMouseReleased(e -> setExpanded(!isExpanded()));
        setExpanded(true);
    }
    public final void setExpanded(boolean b) {
        if(b != expanded) {
            expanded = b;
            openClose.getStyleClass().setAll(expanded ? "fgopen" : "fgclosed");
            getChildren().remove(contents);
            if(expanded)
                getChildren().add(contents);
            openClose.setRotate(expanded ? 90 : 0);
            resize(getPrefWidth(), expanded ? getPrefHeight() : 30);
            outer.controller.adjustArcs();
        }
    }
    public boolean isExpanded() {
        return expanded;
    }
    static final private Image closeArrow = new Image(FGNode.class.getResourceAsStream("CloseArrow.png"));

}
