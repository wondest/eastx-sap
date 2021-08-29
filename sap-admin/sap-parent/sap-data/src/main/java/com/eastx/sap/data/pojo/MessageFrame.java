package com.eastx.sap.data.pojo;

import com.eastx.sap.util.JsonUtils;
import lombok.Data;

/**
 * @ClassName MessageFrame
 * @Description: WebSocket Message Frame
 * @Author Tender
 * @Time 2021/7/11 14:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class MessageFrame {
    /**
     * 消息控制
     */
    private String command;

    /**
     * 消息内容
     */
    private Object content;

    /**
     * 数据
     */
    private static final String COMMAND_DATA = "D";

    /**
     * 结束
     */
    private static final String COMMAND_END = "E";

    /**
     * 开始
     */
    private static final String COMMAND_BEGIN = "B";

    /**
     * 结束消息
     */
    public static MessageFrame MESSAGE_END = MessageFrame.valueOf(COMMAND_END, null);

    /**
     * 开始消息
     */
    public static MessageFrame MESSAGE_BEGIN = MessageFrame.valueOf(COMMAND_BEGIN, null);

    /**
     *
     * @param command
     * @param data
     * @return
     */
    private static MessageFrame valueOf(String command, Object data) {
        MessageFrame frame = new MessageFrame();

        frame.setCommand(command);
        frame.setContent(data);

        return frame;
    }

    public static MessageFrame dataOf(Object data) {
        return MessageFrame.valueOf(COMMAND_DATA, data);
    }

    public String toJson() {
        return JsonUtils.toJson(this);
    }
}
