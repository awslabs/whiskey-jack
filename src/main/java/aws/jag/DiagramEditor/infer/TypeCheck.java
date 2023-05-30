/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.infer;

import aws.jag.DiagramEditor.nodegraph.*;
import java.util.function.*;

public class TypeCheck {
    public void Scan(Graph g) {
        var sb = new StringBuilder();
        var errors = g.typeMismatches;
        errors.clear();
        g.forEachArc((Consumer<Arc>)(Arc a) -> {
            sb.setLength(0);
            var src = a.oneEnd();
            var dst = a.otherEnd();
            var ok = true;
            var ec = ErrorCode.allIsWell;
            if(!src.getType().compatibleWith(dst.getType())) {
                sb.append("Type mismatch ").append(src.getType().getName())
                        .append(" to ").append(dst.getType().getName());
                ok = false;
                ec = ErrorCode.typeMismatch;
            }
            if(!src.getDomain().compatibleWith(dst.getDomain())) {
                if(!sb.isEmpty()) sb.append(";\n ");
                sb.append("Cross domain ").append(src.getDomain())
                        .append(" to ").append(dst.getDomain());
                ok = false;
                ec = ErrorCode.crossDomain;
            }
            if(!ok) errors.add(a);
            if(!sb.isEmpty())
                a.setMessage(ec, sb.toString());
        });
        g.allOK = errors.isEmpty();
        g.otherErrors.clear();
        Consumer<Port> vp = p -> {
            if(p.getName().equals("b"))
                System.out.println(p.getFullName()+" "+p.isUnboundOK()+" "+p.getValue()+" "+p.isConnected());
            if(p.getValue() == null && !p.isConnected()
                    && !p.isUnboundOK()) {
                p.setMessage(ErrorCode.valueExpected, "This port must be given a value");
                g.allOK = false;
                g.otherErrors.add("Port "+p.getName()+" must be given a value");
            }
            else p.setMessage(null);
        };
        g.forEachPort(vp);
    }
}
