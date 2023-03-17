/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.infer;

import aws.jag.DiagramEditor.infer.Scanner;
import aws.jag.DiagramEditor.metadata.*;
import aws.jag.DiagramEditor.nodegraph.*;
import java.util.*;

public class InferIntermediates implements Scanner {
    @Override
    public <T extends Graph> void Scan(T g) {
        System.out.println("Inferring intermediates");
        if(g.typeMismatches.isEmpty()) {
            System.out.println("No type mismatches to fix!");
            return;
        }
        for(Arc a: (List<Arc>) g.typeMismatches) {
            System.out.println("Attempting to solve " + a.getMessage());
            var origin = a.oneEnd();
            var target = a.otherEnd();
            NodeLibrary.singleton.forEachNodeThatTakes(origin.getDomain(), origin.getType(),
                    mn -> {
                System.out.println("\t" + mn);
            });
//            var possibilities = NodeLibrary.singleton.getNodesThatTake(origin.getDomain(),origin.getType());
//            System.out.println(a.getName()+": "+origin+"->"+target);
//            for(var mn:possibilities) System.out.println("\t"+mn);
////            possibilities.forEach((MetaNode mn)->{System.out.println("\t"+mn);});
        };
    }
}
