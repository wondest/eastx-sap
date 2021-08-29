import React, { useState, useRef, useEffect } from 'react';
import { Button, Space, Radio } from 'antd';
import { DownloadOutlined } from '@ant-design/icons';
import { Alert, Card } from 'antd';
import { PageContainer } from '@ant-design/pro-layout';

const Test: React.FC = () => {
    const [count, setCount] = useState(0);
    const [temp, setTemp] = useState(5);
    const latestCount = useRef(count);

    const log=()=>{
        setTimeout(()=>{
            console.info("3秒前temp=5, 现在temp=", temp);
        }, 3000);
    }

    useEffect(()=>{
        console.info('change latestCount');
        latestCount.current = count;

        setTimeout(()=>{
            console.log(`You clicked ${latestCount.current} times`);
        },3000);
    });

    useEffect(()=>{
        console.info("latestCount is change!")
    },[latestCount]);
  
    return (
        <PageContainer>
            <Card>
              <Space>
                <Button type="primary" icon={<DownloadOutlined />} onClick={()=>{setCount(count+1)}}>Increase Count</Button>
                <Button type="primary" icon={<DownloadOutlined />} onClick={()=>{log();setTemp(3);}}>Test Capture</Button>
              </Space>
            </Card>
            <Card>
              <p>You clicked {count} times</p>
            </Card>
        </PageContainer>
    );
};

export default Test;