package helpers

type MultiMap[K, V] = Map[K, List[V]]

object MultiMap:
    def empty[K, V]: MultiMap[K, V] = Map.empty

    def apply[K, V](pairs: Iterable[(K, V)]): MultiMap[K, V] =
        pairs.groupMap(_._1)(_._2).view.mapValues(_.toList).toMap
end MultiMap

extension [K, V](mm: MultiMap[K, V])
    def values: Set[V] = mm.values.flatten.toSet

    def add(key: K, value: V): MultiMap[K, V] =
        mm.updatedWith(key)(_.map(value +: _).orElse(Some(List(value))))

    def addAll(key: K, values: Iterable[V]): MultiMap[K, V] =
        mm.updatedWith(key)(_.map(_ ++ values).orElse(Some(values.toList)))

    def remove(key: K, value: V): MultiMap[K, V] =
        mm.updatedWith(key)(_.map(_.filterNot(_ == value)).filter(_.nonEmpty))

    def removeAll(key: K, values: List[V]): MultiMap[K, V] =
        mm.updatedWith(key)(_.map(_.filterNot(values.contains)).filter(_.nonEmpty))
end extension
