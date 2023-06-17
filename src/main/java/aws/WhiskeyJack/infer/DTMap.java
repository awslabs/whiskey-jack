/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.infer;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;


public class DTMap<T> {
    private Map<Domain,Map<Type,T>> dtInMap;
    public T getExact(Domain d, Type t) {
        var v = getExact0(d,t);
        System.out.println(d+","+t+" => "+v);
        return v;
    }
    public T getExact0(Domain d, Type t) {
        if(dtInMap==null) return null;
        var m0 = dtInMap.get(d);
        if(m0==null) return null;
        return m0.get(t);
    }
    public T getExact(Port p) {
        return getExact(p.getDomain(), p.getType());
    }
    public void putExact(Domain d, Type t, T v) {
        if(d != Domain.any && t!=Type.any) {
        System.out.println(d+","+t+" <= "+v);
        if(dtInMap==null) dtInMap = new HashMap<>();
        var m0 = dtInMap.get(d);
        if(m0==null) dtInMap.put(d, m0 = new HashMap<>());
        m0.put(t, v);
        }
    }
    public void putExact(Port p, T v) {
        putExact(p.getDomain(), p.getType(), v);
    }
    public void clear() { dtInMap = null; }
}
