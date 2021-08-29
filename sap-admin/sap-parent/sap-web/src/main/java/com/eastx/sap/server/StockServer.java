package com.eastx.sap.server;

import com.eastx.sap.data.pojo.MessageFrame;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @ClassName StockServer
 * @Description: 提供WebSocket服务端服务:/stock
 * @Author Tender
 * @Time 2021/7/7 1:00
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@ServerEndpoint(value = "/stock")
@Component
public class StockServer {
    /**
     * 记录当前在线连接数
     */
    private static AtomicInteger onlineCount = new AtomicInteger(0);

    /**
     * 记录所有会话
     */
    private static Map<String, Session> clients = new ConcurrentHashMap<String, Session>();

    /**
     * 连接建立成功调用的方法
     */
    @OnOpen
    public void onOpen(Session session) {
        onlineCount.incrementAndGet(); // 在线数加1
        clients.put(session.getId(), session);
        log.info("有新连接加入：id={}，当前在线人数为：count={}", session.getId(), onlineCount.get());
    }

    /**
     * 连接关闭调用的方法
     */
    @OnClose
    public void onClose(Session session) {
        onlineCount.decrementAndGet(); // 在线数减1
        clients.remove(session.getId());
        log.info("有一连接关闭：id={}，当前在线人数为：count={}", session.getId(), onlineCount.get());
    }

    /**
     * 收到客户端消息后调用的方法
     *
     * @param message 客户端发送过来的消息
     * @param session 客户端会话
     */
    @OnMessage
    public void onMessage(String message, Session session) {
        log.debug("服务端收到客户端[id={}]的消息:{}", session.getId(), message);
    }

    @OnError
    public void onError(Session session, Throwable error) {
        log.error("连接发生错误", error);
    }

    /**
     * 服务端发送消息给客户端
     * @param message
     * @param session
     */
    public static void sendMessage(String message, Session session) {
        try {
            log.info("服务端给客户端[id={}]发送消息:{}", session.getId(), message);
            session.getBasicRemote().sendText(message);
        } catch (Exception e) {
            log.error("服务端发送消息给客户端失败：{}", e);
        }
    }

    /**
     * 服务端发送消息给客户端
     */
    public static void broadcast(Object message) {
        log.info("服务端广播消息:{}", message);
        clients.values().stream().forEach(s -> s.getAsyncRemote().sendText(MessageFrame.dataOf(message).toJson()));
    }

    /**
     * 服务端发送消息给客户端
     */
    public static void broadcastEnd(Object dummy) {
        log.info("服务端广播结束消息:{}");
        clients.values().stream().forEach(s -> s.getAsyncRemote().sendText(MessageFrame.MESSAGE_END.toJson()));
    }

    /**
     * 服务端发送消息给客户端
     */
    public static void broadcastBegin(Object dummy) {
        log.info("服务端广播开始消息:{}");
        clients.values().stream().forEach(s -> s.getAsyncRemote().sendText(MessageFrame.MESSAGE_BEGIN.toJson()));
    }
}