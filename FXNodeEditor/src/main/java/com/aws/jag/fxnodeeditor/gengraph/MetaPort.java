/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

public class MetaPort implements Named, MetaPart {
    public String name;
    public String description;
    public MetaNode node;
    public Type type;
    public Object defaultValue;
    public boolean in;
    @Override
    public String getName() {
        return name;
    }
}
