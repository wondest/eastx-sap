#1 概念

1. RuleEngine     --  定义规则的执行策略

2. RuleExecutor   --  解析规则，执行动作

3. RuleSet,Rule   --  定义规则语义，定义规则动作

4. Parameter      --  参数，考虑配置化，一次性初始化

5. Fact           --  事实，这个是可变的

#2 语义
  模式1：MVEL/SPEL    expression string
  模式2：JAVA         expression object
 
#3 事实数据穿透
  传入事实  ->   执行规则   ->    改造事实 
## 3.1 JAVA

   ruleEngine -> ruleExecutor -> rule.getExpression -> evaluator -> parameter

   归一化返回：

   构建<F>   ParameterBuilder,EvaluatorBuilder
   执行<F> -> <Object> -> <F>

