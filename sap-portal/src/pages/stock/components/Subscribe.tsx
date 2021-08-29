import React, { useState, useRef, useEffect } from 'react';
import { Alert } from 'antd';

export type SubscribeProps = {
  url: string;
  onMessage: (message : string)=>void;
};

export type SubscribeMessage = {
  command: string;
  date: {};
}

const CLOSED: number = 3;
const CLOSING: number = 2;
const CONNECTING: number = 0;
const OPEN: number = 1;
const INITIALIZING: number = 10;

const stateMessage = (state : number) => {
  switch(state) {
    case INITIALIZING:
      return '初始化中';
    case CONNECTING:
      return '连接中';
    case OPEN:
      return '通讯中';
    case CLOSING:
      return '连接正在关闭';
    case CLOSED:
      return '已关闭';
    default:
      return '未知';
  }
}

const Subscribe: React.FC<SubscribeProps> = (props) => {
  const [state, setState] = useState(INITIALIZING);
  const ws = useRef<WebSocket | null>(null);

  //启动时候初始化
  useEffect(() => {
    ws.current = new WebSocket(props.url);

    ws.current.onopen = (e) => {
      setState(ws.current?.readyState ?? INITIALIZING);
    };

    ws.current.onmessage = (e) => {
        props.onMessage(e.data);
    };

    ws.current.onclose = (e) => {
      setState(ws.current?.readyState ?? INITIALIZING);
    };

    ws.current.onerror = (e) => {
      setState(ws.current?.readyState ?? INITIALIZING);
    };

    return () => {
      ws.current?.close();
    };
  }, []);
  
  return (
    <Alert message={stateMessage(state)} type='success' showIcon banner style={{margin: -12, marginBottom: 48}}/>
  );
};

export default Subscribe;