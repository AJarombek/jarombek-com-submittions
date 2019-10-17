### Kibana Developer Tools Commands

```
POST _analyze
{
  "analyzer": "standard",
  "text": "Hello my name is Andy."
}
```

```
POST test/_analyze
{
  "analyzer": "custom_analyzer",
  "text": "😀"
}
```