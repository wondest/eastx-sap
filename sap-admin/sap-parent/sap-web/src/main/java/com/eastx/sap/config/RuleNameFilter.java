package com.eastx.sap.config;

import org.kie.api.runtime.rule.AgendaFilter;
import org.kie.api.runtime.rule.Match;

/**
 * @ClassName RuleNameFilter
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/12 9:36
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleNameFilter implements AgendaFilter {

    String ruleName;

    RuleNameFilter(String ruleName) {
        this.ruleName = ruleName;
    }

    @Override
    public boolean accept(Match match) {
        //System.out.println(match.getRule().getName());
        //System.out.println(match.getRule().getPackageName());
        //System.out.println(match.getRule().getNamespace());
        return match.getRule().getName().equals(ruleName);
    }
}
