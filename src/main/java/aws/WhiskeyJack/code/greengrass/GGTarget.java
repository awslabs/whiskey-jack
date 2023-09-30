/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.code.greengrass;

import aws.WhiskeyJack.code.*;
import java.util.*;


public class GGTarget extends CodeTarget {
    public GGTarget(OuterBuildController c) {
        super(c);
        preComment = null;
        postComment = null;
        onComment = "# ";
    }
    @Override
    public String getCodeDirectoryName() {
        return "greengrass";
    }
    @Override
    public CodeTarget comment(Collection<String> s) {
        if(s != null)
            comments.addAll(s);
        return this;
    }
    final Collection<String> comments = new ArrayList<>();
    public void dumpComments() {
        super.comment(comments);
        comments.clear();
    }
    @Override
    public String getDestinationName() {
        return "recipe";
    }
    @Override
    public String getDestinationExtension() {
        return "yaml";
    }
}
