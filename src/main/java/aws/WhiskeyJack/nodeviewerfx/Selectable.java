/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.nodeviewerfx;

public interface Selectable {
    public javafx.scene.Node getView();
    public void delete();
    public void setTag(String tag);
    default public boolean canDrag() { return false; }
    default public void endDrag() {
        var v = getView();
        v.setLayoutX(v.getLayoutX() + v.getTranslateX());
        v.setLayoutY(v.getLayoutY() + v.getTranslateY());
        v.setTranslateX(0);
        v.setTranslateY(0);
    }
    default public void setDrag(double dx, double dy) {
        var v = getView();
        v.setTranslateX(dx);
        v.setTranslateY(dy);
    }
}
