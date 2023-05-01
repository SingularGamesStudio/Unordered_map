#pragma once

#include <algorithm>
#include <cmath>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <optional>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T, typename Allocator = std::allocator<T>>
class List {
    class BaseNode {
       public:
        BaseNode* next;
        BaseNode* prev;

        BaseNode() : next(this), prev(this) {}

        BaseNode(const BaseNode& other) : next(other.next), prev(other.prev) {}
        BaseNode& operator=(const BaseNode& other) {
            if (&other == this) return *this;
            next = other.next;
            prev = other.prev;
            return *this;
        }

        ~BaseNode() {
            prev->next = next;
            next->prev = prev;
        }
    };
    class Node : public BaseNode {
       public:
        T value;
    };
    Allocator alloc;
    using nodeAllocType =
        typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;

    BaseNode end_node;
    size_t sz;
    nodeAllocType node_alloc;

    Node* allocNode() {
        return std::allocator_traits<nodeAllocType>::allocate(node_alloc, 1);
    }

    void deallocNode(Node* node) {
        std::allocator_traits<nodeAllocType>::deallocate(node_alloc, node, 1);
    }

    void push_back_empty() {
        Node* node = allocNode();
        try {
            std::allocator_traits<Allocator>::construct(alloc, &(node->value));
        } catch (...) {
            deallocNode(node);
            throw;
        }
        node->prev = end_node.prev;
        node->next = &end_node;
        node->next->prev = node;
        node->prev->next = node;
        sz++;
    }

   public:
    template <typename T1>
    class BaseIterator {
        friend class List;

       private:
       public:
        List::BaseNode* node;
        BaseIterator(List::BaseNode* ptr) : node(ptr) {}
        BaseIterator() {}
        BaseIterator(const BaseIterator& other) = default;
        using difference_type = int;
        using value_type = T1;
        using reference = T1&;
        using pointer = T1*;
        using iterator_category = std::bidirectional_iterator_tag;

        BaseIterator& operator++() {
            node = node->next;
            return *this;
        }

        BaseIterator operator++(int) {
            BaseIterator copy = *this;
            ++*this;
            return copy;
        }
        BaseIterator& operator--() {
            node = node->prev;
            return *this;
        }

        BaseIterator operator--(int) {
            BaseIterator copy = *this;
            --*this;
            return copy;
        }

        bool operator==(const BaseIterator& other) const = default;

        T1& operator*() const {
            return *reinterpret_cast<T1*>(&(static_cast<Node*>(node)->value));
        }
        T1* operator->() const {
            return reinterpret_cast<T1*>(&(static_cast<Node*>(node)->value));
        }

        operator BaseIterator<const T1>() const {
            BaseIterator<const T1> res(node);
            return res;
        }

        template <typename Key, typename Value, typename Hash, typename Equal,
                  typename Alloc>
        friend class UnorderedMap;
    };

    void clear() {
        while (sz > 0) {
            pop_back();
        }
    }

    List(Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {}
    List(size_t n, const T& value, Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {
        try {
            for (size_t i = 0; i < n; i++) {
                push_back(value);
            }
        } catch (...) {
            clear();
            throw;
        }
    }
    List(size_t n, Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {
        try {
            for (size_t i = 0; i < n; i++) {
                push_back_empty();
            }
        } catch (...) {
            clear();
            throw;
        }
    }
    // here clang-tidy does not see initialization of sz
    // NOLINTNEXTLINE
    List() : List(Allocator()) {}
    // NOLINTNEXTLINE
    List(size_t n, const T& value) : List(n, value, Allocator()) {}
    // NOLINTNEXTLINE
    List(size_t n) : List(n, Allocator()) {}

    List(const List& other)
        : alloc(std::allocator_traits<
                Allocator>::select_on_container_copy_construction(other.alloc)),
          end_node(BaseNode()),
          sz(0),
          node_alloc(alloc) {
        if (&other == this) return;
        try {
            for (const_iterator it = other.begin(); it != other.end(); it++) {
                push_back(*it);
            }
        } catch (...) {
            clear();
            throw;
        }
    }

    List& operator=(const List& other) {
        if (&other == this) return *this;
        size_t cnt = 0;
        try {
            for (const_iterator it = other.begin(); it != other.end(); it++) {
                push_back(*it);
                cnt++;
            }
        } catch (...) {
            for (size_t i = 0; i < cnt; i++) pop_back();
            throw;
        }
        while (sz > cnt) pop_front();

        if (std::allocator_traits<
                Allocator>::propagate_on_container_copy_assignment::value) {
            alloc = other.alloc;
            node_alloc = other.node_alloc;
        }
        return *this;
    }

    Allocator& get_allocator() { return alloc; }
    const Allocator& get_allocator() const { return alloc; }

    using iterator = BaseIterator<T>;
    using const_iterator = BaseIterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() { return iterator(end_node.next); }
    const_iterator begin() const { return cbegin(); }
    iterator end() { return iterator(&end_node); }
    const_iterator end() const { return cend(); }
    const_iterator cbegin() const { return const_iterator(end_node.next); }
    const_iterator cend() const { return const_iterator(end_node.next->prev); }
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return crbegin(); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { return crend(); }
    const_reverse_iterator crbegin() const {
        return const_reverse_iterator(cend());
    }
    const_reverse_iterator crend() const {
        return const_reverse_iterator(cbegin());
    }

    size_t size() const { return sz; }

    template <typename U>
    iterator insert(const_iterator pos, U&& value) {
        Node* node = allocNode();
        try {
            std::allocator_traits<Allocator>::construct(alloc, &(node->value),
                                                        std::forward<U>(value));
        } catch (...) {
            deallocNode(node);
            throw;
        }
        node->prev = pos.node->prev;
        node->next = pos.node;
        node->next->prev = node;
        node->prev->next = node;
        sz++;
        return iterator(node);
    }

    template <typename U>
    void push_back(U&& value) {
        insert(const_iterator(&end_node), std::forward<U>(value));
    }

    template <typename U>
    void push_front(U&& value) {
        insert(const_iterator(end_node.next), std::forward<U>(value));
    }

    iterator erase(const_iterator pos) {
        Node* node = static_cast<Node*>(pos.node);
        node->~Node();
        iterator res(node->next);
        deallocNode(node);
        sz--;
        return res;
    }

    void swap(List& other) {
        std::swap(end_node, other.end_node);
        std::swap(sz, other.sz);

        if (end_node.next != &other.end_node) {
            end_node.next->prev = &end_node;
            end_node.prev->next = &end_node;
        } else {
            end_node.next = &end_node;
            end_node.prev = &end_node;
        }

        if (other.end_node.next != &end_node) {
            other.end_node.next->prev = &other.end_node;
            other.end_node.prev->next = &other.end_node;
        } else {
            other.end_node.next = &other.end_node;
            other.end_node.prev = &other.end_node;
        }

        if (std::allocator_traits<
                Allocator>::propagate_on_container_swap::value) {
            std::swap(alloc, other.alloc);
            std::swap(node_alloc, other.node_alloc);
        }
    }

    void pop_front() { erase(iterator(end_node.next)); }
    void pop_back() { erase(iterator(end_node.prev)); }

    ~List() { clear(); }

    template <typename Key, typename Value, typename Hash, typename Equal,
              typename Alloc>
    friend class UnorderedMap;
};
/*















*/

template <typename Key, typename Value, typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {
   public:
    using NodeType = std::pair<Key, Value>;

   private:
    struct Node {
        NodeType data;
        size_t hash;

        template <typename U>
        Node(U&& data, size_t hash) : data(std::forward<U>(data)), hash(hash) {}

        template <typename... Args>
        Node(const Hash& hash, Args&&... args)
            : data(std::forward<Args>(args)...), hash(hash(data.first)) {}
    };

    using nodeAllocType =
        typename std::allocator_traits<Alloc>::template rebind_alloc<Node>;

    nodeAllocType alloc;
    Hash hash;
    Equal equal;

    size_t sz = 0;
    size_t bucket_cnt = 1;
    float max_load = 1.0;

    List<Node, Alloc> data;

    std::vector<std::optional<typename List<Node, Alloc>::iterator>> buckets;

    Node* allocNode() {
        return std::allocator_traits<nodeAllocType>::allocate(alloc, 1);
    }

    void deallocNode(Node* node) {
        std::allocator_traits<nodeAllocType>::deallocate(alloc, node, 1);
    }

    template <typename T>
    class BaseIterator {
        friend class UnorderedMap;

       private:
        List<UnorderedMap::Node, Alloc>::iterator node;
        BaseIterator(List<UnorderedMap::Node, Alloc>::iterator ptr)
            : node(ptr) {}

       public:
        BaseIterator() {}
        BaseIterator(const BaseIterator& other) = default;

        using difference_type = int;
        using value_type = T;
        using reference = T&;
        using pointer = T*;
        using const_reference = const T&;
        using const_pointer = const T*;
        using iterator_category = std::bidirectional_iterator_tag;

        BaseIterator& operator++() {
            ++node;
            return *this;
        }

        BaseIterator operator++(int) {
            BaseIterator copy = *this;
            ++*this;
            return copy;
        }
        BaseIterator& operator--() {
            --node;
            return *this;
        }

        BaseIterator operator--(int) {
            BaseIterator copy = *this;
            --*this;
            return copy;
        }

        bool operator==(const BaseIterator& other) const = default;

        T& operator*() const { return (*node).data; }
        T* operator->() const { return &((*node).data); }

        size_t gethash() const { return (*node).hash; }

        operator BaseIterator<const T>() const {
            BaseIterator<const T> res(node);
            return res;
        }
    };

    bool cmp(List<Node, Alloc>::Node* a, List<Node, Alloc>::Node* b) {
        return a->value.hash < b->value.hash;
    }

   public:
    Alloc get_allocator() { return alloc; }
    const Alloc& get_allocator() const { return alloc; }

    using iterator = BaseIterator<NodeType>;
    using const_iterator = BaseIterator<const NodeType>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() { return iterator(data.end_node.next); }
    const_iterator begin() const { return cbegin(); }
    iterator end() { return iterator(&data.end_node); }
    const_iterator end() const { return cend(); }
    const_iterator cbegin() const { return const_iterator(data.end_node.next); }
    const_iterator cend() const {
        return const_iterator(data.end_node.next->prev);
    }
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return crbegin(); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { return crend(); }
    const_reverse_iterator crbegin() const {
        return const_reverse_iterator(cend());
    }
    const_reverse_iterator crend() const {
        return const_reverse_iterator(cbegin());
    }

    size_t size() { return sz; }

    UnorderedMap(size_t bucket_cnt = 1, const Hash& hash = Hash(),
                 const Equal& equal = Equal(), const Alloc& alloc = Alloc())
        : alloc(alloc),
          hash(hash),
          equal(equal),

          sz(0),
          bucket_cnt(bucket_cnt),
          max_load(1.0),
          data(List<Node, Alloc>(alloc)),
          buckets(
              std::vector<std::optional<typename List<Node, Alloc>::iterator>>(
                  bucket_cnt, std::nullopt)) {}

    UnorderedMap(const UnorderedMap& other)
        : alloc(std::allocator_traits<
                Alloc>::select_on_container_copy_construction(other.alloc)),
          hash(other.hash),
          equal(other.equal),
          sz(other.sz),
          bucket_cnt(other.bucket_cnt),
          max_load(other.max_load),
          data(other.data),
          buckets(
              std::vector<std::optional<typename List<Node, Alloc>::iterator>>(
                  bucket_cnt, std::nullopt)) {
        for (typename List<Node, Alloc>::iterator it = data.begin();
             it != data.end(); it++) {
            size_t at = (*it).hash;
            if (buckets[at] == std::nullopt) {
                buckets[at] = it;
            }
        }
    }

    UnorderedMap(const UnorderedMap&& other)
        : alloc(std::move(other.alloc)),
          hash(std::move(other.hash)),
          equal(std::move(other.equal)),
          sz(std::move(other.sz)),
          bucket_cnt(std::move(other.bucket_cnt)),
          max_load(std::move(other.max_load)),
          data(std::move(other.data)),
          buckets(std::move(buckets)) {}

    UnorderedMap& operator=(const UnorderedMap& other) {
        if (this == &other) return *this;
        UnorderedMap temp(other);
        swap(temp);
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        if (this == &other) return *this;
        swap(other);
        return *this;
    }

   private:
    std::pair<const_iterator, bool> lower_bound(const Key& key) const {
        size_t khash = hash(key) % bucket_cnt;
        const_iterator it = end();
        if (buckets[khash] != std::nullopt) {
            it = const_iterator(buckets[khash].value().node);
            while (1) {
                if (it.gethash() != khash) break;
                if (equal(key, (*it).first)) return {it, true};
                it++;
            }
            it = const_iterator(buckets[khash].value().node);
        }
        return {it, false};
    }

    std::pair<iterator, bool> lower_bound(const Key& key) {
        size_t khash = hash(key) % bucket_cnt;
        iterator it = end();
        if (buckets[khash] != std::nullopt) {
            it = iterator(buckets[khash].value().node);
            while (1) {
                if (it.gethash() != khash) break;
                if (equal(key, (*it).first)) return {it, true};
                it++;
            }
            it = iterator(buckets[khash].value().node);
        }
        return {it, false};
    }

    template <typename U>
    iterator insert_node(U&& what, iterator next) {
        Node* ins = std::allocator_traits<nodeAllocType>::allocate(alloc, 1);
        iterator res;
        try {
            std::allocator_traits<nodeAllocType>::construct(alloc, ins, what,
                                                            hash(what.first));
            res = iterator(data.insert(next.node, std::move(*ins)));
        } catch (...) {
            std::allocator_traits<nodeAllocType>::deallocate(alloc, ins, 1);
            throw;
        }
        sz++;
        std::allocator_traits<nodeAllocType>::deallocate(alloc, ins, 1);
        rehash(bucket_cnt);
        return res;
    }

   public:
    std::pair<iterator, bool> insert(const NodeType& what) {
        auto where = lower_bound(what.first);
        if (where.second) return {where.first, false};
        return {insert_node(what, where.first), true};
    }

    std::pair<iterator, bool> insert(NodeType&& what) {
        auto where = lower_bound(what.first);
        if (where.second) return {where.first, false};
        return {insert_node(std::move(what), where.first), true};
    }

    template <typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        printstruct();
        Node* ins = std::allocator_traits<nodeAllocType>::allocate(alloc, 1);
        iterator res(data.end());
        try {
            std::allocator_traits<nodeAllocType>::construct(
                alloc, ins, hash, std::forward<Args>(args)...);
            auto where = lower_bound(ins->data.first);
            if (where.second) {
                std::allocator_traits<nodeAllocType>::deallocate(alloc, ins, 1);
                return {where.first, false};
            }
            res = iterator(data.insert(where.first.node, std::move(*ins)));
        } catch (...) {
            std::allocator_traits<nodeAllocType>::deallocate(alloc, ins, 1);
            throw;
        }
        sz++;
        std::allocator_traits<nodeAllocType>::deallocate(alloc, ins, 1);
        rehash(bucket_cnt);
        return {res, true};
    }

    void erase(iterator where) { data.erase(where.node); }

    void erase(iterator first, iterator last) {
        for (auto it = first; it != last; it++) {
            erase(it);
        }
    }

    const_iterator find(const Key& key) const {
        auto temp = lower_bound(key);
        if (temp.second) {
            return temp.first;
        }
        return end();
    }

    iterator find(const Key& key) {
        auto temp = lower_bound(key);
        if (temp.second) {
            return temp.first;
        }
        return end();
    }

    template <class InputIt>
    void insert(InputIt first, InputIt last) {
        std::vector<iterator> added;
        try {
            for (auto it = first; it != last; it++) {
                auto temp = insert(std::move(*it));
                if (temp.second) added.push_back(temp.first);
            }
        } catch (...) {
            for (iterator z : added) {
                erase(z);
            }
            throw;
        }
    }

    Value& at(const Key& key) {
        auto where = lower_bound(key);
        if (where.second) {
            return where.first->second;
        } else
            throw std::out_of_range("UnorderedMap index out of range");
    }

    const Value& at(const Key& key) const {
        auto where = lower_bound(key);
        if (where.second) {
            return where.first->second;
        } else
            throw std::out_of_range("UnorderedMap index out of range");
    }

    Value& operator[](const Key& key) {
        auto where = lower_bound(key);
        if (where.second) {
            return where.first->second;
        } else {
            return insert_node(NodeType(key, Value()), where.first)->second;
        }
    }
    Value& operator[](Key&& key) {
        auto where = lower_bound(key);
        if (where.second) {
            return where.first->second;
        } else {
            return insert_node(NodeType(std::move(key), Value()), where.first)
                ->second;
        }
    }

    float load_factor() const {
        return static_cast<float>(sz) / static_cast<float>(bucket_cnt);
    }

    float max_load_factor() const { return max_load; }

    void max_load_factor(float newVal) {
        max_load = newVal;
        if (load_factor() > max_load) {
            rehash(0);
        }
    }

    void printstruct() {
        std::cerr << bucket_cnt << std::endl;
        for (size_t i = 0; i < bucket_cnt; i++) {
            if (!buckets[i])
                std::cerr << 0 << " ";
            else {
                int cnt = 0;
                auto it = iterator(buckets[i].value());
                while (it != end() && it.gethash() == i) {
                    cnt++;
                    it++;
                }
                std::cerr << cnt << " ";
            }
        }
        std::cerr << std::endl;
    }

    void reserve(size_t count) {
        rehash(std::ceil(static_cast<float>(count) / max_load));
    }

    void rehash(size_t count) {
        if (max_load * static_cast<float>(count) < static_cast<float>(sz)) {
            count =
                static_cast<size_t>(static_cast<float>(sz * 2) / max_load) + 1;
        }
        if (count == bucket_cnt) return;
        std::vector<typename List<Node, Alloc>::Node*> nodes;
        buckets.clear();
        if (data.size() > 0) {
            nodes.push_back(static_cast<typename List<Node, Alloc>::Node*>(
                data.end_node.next));
            while (true) {
                typename List<Node, Alloc>::BaseNode* next = nodes.back()->next;
                if (next == &data.end_node) break;
                nodes.push_back(
                    static_cast<typename List<Node, Alloc>::Node*>(next));
            }
        }

        data.end_node.next = &(data.end_node);
        data.end_node.prev = &(data.end_node);
        buckets =
            std::vector<std::optional<typename List<Node, Alloc>::iterator>>(
                count, std::nullopt);
        for (typename List<Node, Alloc>::Node* node : nodes) {
            node->value.hash = hash(node->value.data.first) % count;
            size_t at = node->value.hash;
            typename List<Node, Alloc>::BaseNode* next = &data.end_node;
            if (buckets[at] != std::nullopt) {
                next = buckets[at].value().node;
            }
            node->next = next;
            node->prev = next->prev;
            node->prev->next = node;
            node->next->prev = node;
            buckets[at] = typename List<Node, Alloc>::iterator(node);
        }
    }

    void swap(UnorderedMap& other) {
        std::swap(hash, other.hash);
        std::swap(equal, other.equal);
        std::swap(sz, other.sz);
        std::swap(bucket_cnt, other.bucket_cnt);
        std::swap(max_load, other.max_load);
        data.swap(other.data);
        std::swap(buckets, other.buckets);
        if (std::allocator_traits<Alloc>::propagate_on_container_swap::value)
            std::swap(alloc, other.alloc);
    }

    ~UnorderedMap() = default;
};