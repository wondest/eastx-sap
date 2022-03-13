package com.eastx.sap.rule;

import org.kie.api.definition.type.Label;

import java.io.Serializable;

/**
 * @ClassName DroolsMessage
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/12 9:43
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DroolsMessage implements Serializable {
    public static final int HELLO   = 0;
    public static final int GOODBYE = 1;

    @Label("消息")
    private String  message;

    @Label("状态")
    private int     status;

    @Label("状态1")
    private int     loanamt;

    @Label("状态2")
    private int     loanbal;

    @Label("结果")
    private String  result;

    public DroolsMessage(String message, int status) {
        this.message = message;
        this.status = status;
    }


    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public int getLoanamt() {
        return loanamt;
    }

    public void setLoanamt(int loanamt) {
        this.loanamt = loanamt;
    }

    public int getLoanbal() {
        return loanbal;
    }

    public void setLoanbal(int loanbal) {
        this.loanbal = loanbal;
    }

    @Override
    public String toString() {
        return "DroolsMessage{" +
                "message='" + message + '\'' +
                ", status=" + status +
                ", loanamt=" + loanamt +
                ", loanbal=" + loanbal +
                ", result='" + result + '\'' +
                '}';
    }
}
