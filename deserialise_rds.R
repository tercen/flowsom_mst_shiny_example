# get model
# Second step
# http://127.0.0.1:5402/admin/w/8f87fbc2db7b31eb2cf5d59c300052b2/ds/e5d06f56-c031-4cf9-9ce9-f9c9728f117f
options("tercen.workflowId"= "527f056bd521570e65126f6dbb0091e3")
options("tercen.stepId"= "696ceac5-92e5-4b8b-b76d-06d4a2346a16")

ctx = tercenCtx()

deserialize.from.string = function(str64){
  con = rawConnection(base64enc::base64decode(str64), "r+")
  object = readRDS(con)
  close(con)
  return(object)
}
find.schema.by.factor.name = function(ctx, factor.name){
  visit.relation = function(visitor, relation){
    if (inherits(relation,"SimpleRelation")){
      visitor(relation)
    } else if (inherits(relation,"CompositeRelation")){
      visit.relation(visitor, relation$mainRelation)
      lapply(relation$joinOperators, function(jop){
        visit.relation(visitor, jop$rightRelation)
      })
    } 
    invisible()
  }
  
  myenv = new.env()
  add.in.env = function(object){
    myenv[[toString(length(myenv)+1)]] = object$id
  }
  
  visit.relation(add.in.env, ctx$query$relation)
  
  schemas = lapply(as.list(myenv), function(id){
    ctx$client$tableSchemaService$get(id)
  })
  
  Find(function(schema){
    !is.null(Find(function(column) column$name == factor.name, schema$columns))
  }, schemas);
}

# search for a schema that contains a column name 
# schema = find.schema.by.factor.name(ctx, '.base64.serialized.r.model')
schema = find.schema.by.factor.name(ctx, ctx$labels[[1]])
# get the data
table = ctx$client$tableSchemaService$select(schema$id, Map(function(x) x$name, schema$columns), 0, schema$nRows)

my.models = lapply(as_tibble(table)[[".base64.serialized.r.model"]], deserialize.from.string)
